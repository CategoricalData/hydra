{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Rewriting where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Io                 as Io
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Accessors as Accessors
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Flows as Flows_
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Printing as Printing
import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


rewritingDefinition :: String -> TTerm a -> TElement a
rewritingDefinition = definitionInModule hydraRewritingModule

hydraRewritingModule :: Module
hydraRewritingModule = Module (Namespace "hydra.rewriting") elements
    [Strip.hydraStripModule, Lexical.hydraLexicalModule, Qnames.hydraQnamesModule, Sorting.hydraSortingModule]
    [Tier1.hydraCodersModule, Tier1.hydraMantleModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule] $
    Just ("Utilities for type and term rewriting and analysis.")
  where
   elements = [
     el elementsWithDependenciesDef,
     el expandTypedLambdasDef,
     el flattenLetTermsDef,
     el foldOverTermDef,
     el foldOverTypeDef,
     el freeVariablesInTermDef,
     el freeVariablesInTypeDef,
     el freeVariablesInTypeSchemeSimpleDef,
     el freeVariablesInTypeSchemeDef,
     el freeVariablesInTypeSimpleDef,
     el getTermTypeDef,
     el inlineTypeDef,
     el isFreeVariableInTermDef,
     el isLambdaDef,
     el mapBeneathTypeAnnotationsDef,
     el normalizeTypeVariablesInTermDef,
     el removeTermAnnotationsDef,
     el removeTypeAnnotationsDef,
     el replaceFreeNameDef,
     el rewriteDef,
     el rewriteTermDef,
     el rewriteTermMDef,
     el rewriteTermMetaDef,
     el rewriteTermMetaMDef,
     el rewriteTypeDef,
     el rewriteTypeMDef,
     el rewriteTypeMetaDef,
     el simplifyTermDef,
     el stripTermRecursiveDef,
     el stripTypeRecursiveDef,
     el stripTypeSchemeRecursiveDef,
     el stripTypesFromTermDef,
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

elementsWithDependenciesDef :: TElement ([Element] -> Flow Graph [Element])
elementsWithDependenciesDef = rewritingDefinition "elementsWithDependencies" $
  doc "Get elements with their dependencies" $
  lambda "original" $ lets [
    "depNames">: lambda "el" $ Sets.toList $ ref termDependencyNamesDef @@ true @@ false @@ false @@ (Graph.elementTerm $ var "el"),
    "allDepNames">: Lists.nub $ Lists.concat2
      (Lists.map (unaryFunction Graph.elementName) (var "original"))
      (Lists.concat $ Lists.map (var "depNames") (var "original"))]
    $ Flows.mapList (ref Lexical.requireElementDef) (var "allDepNames")

expandTypedLambdasDef :: TElement (Term -> Term)
expandTypedLambdasDef = rewritingDefinition "expandTypedLambdas" $
  doc "A variation of expandLambdas which also attaches type annotations when padding function terms" $
  lambda "term" $ lets [
    "toNaryFunType">: lambda "typ" $ lets [
      "helper">: lambda "t" $
        match _Type (Just $ pair (list []) (var "t")) [
          _Type_function>>: lambda "ft" $ lets [
            "dom0">: Core.functionTypeDomain $ var "ft",
            "cod0">: Core.functionTypeCodomain $ var "ft",
            "recursive">: var "helper" @@ var "cod0",
            "doms">: first $ var "recursive",
            "cod1">: second $ var "recursive"]
            $ pair (Lists.cons (var "dom0") (var "doms")) (var "cod1")] @@ var "t"]
      $ var "helper" @@ (ref Strip.stripTypeDef @@ var "typ"),
    "getFunType">: lambda "term" $ Optionals.map (var "toNaryFunType") (ref getTermTypeDef @@ var "term"),
    "padTerm">: lambdas ["i", "doms", "cod", "term"] $
      Logic.ifElse (Lists.null $ var "doms")
        (var "term")
        (lets [
          "dom">: Lists.head $ var "doms",
          "var">: Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ var "i"),
          "tailDoms">: Lists.tail $ var "doms",
          "typedTerm">: lambda "typ" $ lambda "term" $ Core.termTyped $ Core.typedTerm (var "term") (var "typ"),
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
              @@ (var "typedTerm" @@ (var "toFunctionType" @@ var "tailDoms" @@ var "cod") @@
                  (Core.termApplication $ Core.application
                    (var "typedTerm" @@ (var "toFunctionType" @@ var "doms" @@ var "cod") @@ var "term")
                    (Core.termVariable $ var "var")))),
    "expand">: lambdas ["doms", "cod", "term"] $
      match _Term (Just $ ref rewriteTermDef @@ var "rewrite" @@ var "term") [
        _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
          (var "expand" @@ var "doms" @@ var "cod" @@ (Core.annotatedTermSubject $ var "at"))
          (Core.annotatedTermAnnotation $ var "at"),
        _Term_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app"]
          $ Optionals.maybe
            (ref rewriteTermDef @@ var "rewrite" @@ var "term")
            (lambda "typ" $ Core.termApplication $ Core.application
              (var "expand" @@ (Lists.cons (var "typ") (var "doms")) @@ var "cod" @@ var "lhs")
              (ref expandTypedLambdasDef @@ var "rhs"))
            (ref getTermTypeDef @@ var "rhs"),
        _Term_function>>: match _Function (Just $ var "padTerm" @@ int32 1 @@ var "doms" @@ var "cod" @@ var "term") [
          _Function_lambda>>: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (var "expand" @@ (Lists.tail $ var "doms") @@ var "cod" @@ (Core.lambdaBody $ var "l"))],
        _Term_let>>: lambda "lt" $ lets [
          "expandBinding">: lambda "b" $ Core.letBinding
            (Core.letBindingName $ var "b")
            (ref expandTypedLambdasDef @@ (Core.letBindingTerm $ var "b"))
            (Core.letBindingType $ var "b")]
          $ Core.termLet $ Core.letExpression
            (Lists.map (var "expandBinding") (Core.letBindings $ var "lt"))
            (var "expand" @@ var "doms" @@ var "cod" @@ (Core.letEnvironment $ var "lt")),
        _Term_typed>>: lambda "tt" $ Core.termTyped $ Core.typedTerm
          (var "expand" @@ var "doms" @@ var "cod" @@ (Core.typedTermTerm $ var "tt"))
          (Core.typedTermType $ var "tt")] @@ var "term",
    "rewrite">: lambdas ["recurse", "term"] $
      Optionals.maybe
        (var "recurse" @@ var "term")
        (lambda "domsAndCod" $ lets [
          "doms">: first $ var "domsAndCod",
          "cod">: second $ var "domsAndCod"]
          $ var "expand" @@ var "doms" @@ var "cod" @@ var "term")
        (var "getFunType" @@ var "term")]
    $ ref rewriteTermDef @@ var "rewrite" @@ var "term"

flattenLetTermsDef :: TElement (Term -> Term)
flattenLetTermsDef = rewritingDefinition "flattenLetTerms" $
  doc "Flatten nested let expressions" $
  lambda "term" $ lets [
    "rewriteBinding">: lambda "binding" $ lets [
      "key0">: Core.letBindingName $ var "binding",
      "val0">: Core.letBindingTerm $ var "binding",
      "t">: Core.letBindingType $ var "binding"]
      $ match _Term (Just $ pair (Core.letBinding (var "key0") (var "val0") (var "t")) (list [])) [
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
            (Lists.map (var "newBinding") (var "bindings1"))] @@ var "val0",
    "flatten">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"]
      $ match _Term (Just $ var "rewritten") [
        _Term_let>>: lambda "lt" $ lets [
          "bindings">: Core.letBindings $ var "lt",
          "body">: Core.letEnvironment $ var "lt",
          "forResult">: lambda "hr" $ Lists.cons (first $ var "hr") (second $ var "hr"),
          "newBindings">: Lists.concat $ Lists.map (var "forResult" <.> var "rewriteBinding") (var "bindings")]
          $ Core.termLet $ Core.letExpression (var "newBindings") (var "body")] @@ var "rewritten"]
    $ ref rewriteTermDef @@ var "flatten" @@ var "term"

foldOverTermDef :: TElement (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTermDef = rewritingDefinition "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  lambda "order" $ lambda "fld" $ lambda "b0" $ lambda "term" $ (match _TraversalOrder Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (ref subtermsDef @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtermsDef @@ var "term"))
      @@ var "term")] @@ var "order")

foldOverTypeDef :: TElement (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverTypeDef = rewritingDefinition "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  lambda "order" $ lambda "fld" $ lambda "b0" $ lambda "typ" $ (match _TraversalOrder Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (ref subtypesDef @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtypesDef @@ var "typ"))
      @@ var "typ")] @@ var "order")

freeVariablesInTermDef :: TElement (Term -> S.Set Name)
freeVariablesInTermDef = rewritingDefinition "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  lambda "term" $ lets [
    "dfltVars">: Phantoms.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (ref freeVariablesInTermDef @@ var "t"))
      @@ Sets.empty
      @@ (ref subtermsDef @@ var "term")]
    $ match _Term (Just $ var "dfltVars") [
      _Term_function>>: match _Function (Just $ var "dfltVars") [
        _Function_lambda>>: lambda "l" (Sets.delete
          (Core.lambdaParameter $ var "l")
          (ref freeVariablesInTermDef @@ (Core.lambdaBody $ var "l")))],
--      TODO: restore the following
--      _Term_let>>: lambda "l" (Sets.difference
--        @@ (ref freeVariablesInTermDef @@ (Core.letEnvironment $ var "l"))
--        @@ (Sets.fromList (Lists.map first (Maps.toList (Core.letBindings $ var "l"))))),
      _Term_variable>>: lambda "v" (Sets.singleton $ var "v")] @@ var "term"

freeVariablesInTypeDef :: TElement (Type -> S.Set Name)
freeVariablesInTypeDef = rewritingDefinition "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  lambda "typ" $ lets [
    "dfltVars">: Phantoms.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (recurse @@ var "t"))
      @@ Sets.empty
      @@ (ref subtypesDef @@ var "typ")]
    $ match _Type (Just $ var "dfltVars") [
      _Type_forall>>: lambda "lt" (Sets.delete
          (Core.forallTypeParameter $ var "lt")
          (recurse @@ (Core.forallTypeBody $ var "lt"))),
      -- TODO: let-types
      _Type_variable>>: lambda "v" (Sets.singleton $ var "v")] @@ var "typ"
  where
    recurse = ref freeVariablesInTypeDef

freeVariablesInTypeSimpleDef :: TElement (Type -> S.Set Name)
freeVariablesInTypeSimpleDef = rewritingDefinition "freeVariablesInTypeSimple" $
  doc "Same as freeVariablesInType, but ignores the binding action of lambda types" $
  lambda "typ" $ lets [
    "helper">: lambdas ["types", "typ"] $
      match _Type (Just $ var "types") [
        _Type_variable>>: lambda "v" $ Sets.insert (var "v") (var "types")] @@ var "typ"]
    $ ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "helper" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSchemeDef :: TElement (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeDef = rewritingDefinition "freeVariablesInTypeScheme" $
  doc "Find free variables in a type scheme" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "t">: Core.typeSchemeType $ var "ts"]
    $ Sets.difference (ref freeVariablesInTypeDef @@ var "t") (Sets.fromList $ var "vars")

freeVariablesInTypeSchemeSimpleDef :: TElement (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeSimpleDef = rewritingDefinition "freeVariablesInTypeSchemeSimple" $
  doc "Find free variables in a type scheme (simple version)" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "t">: Core.typeSchemeType $ var "ts"]
    $ Sets.difference (ref freeVariablesInTypeSimpleDef @@ var "t") (Sets.fromList $ var "vars")

getTermTypeDef :: TElement (Term -> Maybe Type)
getTermTypeDef = rewritingDefinition "getTermType" $
  doc "Get the annotated type of a given term, if any" $
  match _Term (Just nothing) [
    "annotated">: ref getTermTypeDef <.> project _AnnotatedTerm _AnnotatedTerm_subject,
    "typed">: lambda "tt" $ just (project _TypedTerm _TypedTerm_type @@ var "tt")]

inlineTypeDef :: TElement (M.Map Name Type -> Type -> Flow s Type)
inlineTypeDef = rewritingDefinition "inlineType" $
  doc "Inline all type variables in a type using the provided schema. Note: this function is only appropriate for nonrecursive type definitions" $
  lambdas ["schema", "typ"] $ lets [
    "f">: lambdas ["recurse", "typ"] $
      Flows.bind (var "recurse" @@ var "typ") $
        lambda "tr" $
          match _Type (Just $ Flows.pure $ var "tr") [
            _Type_variable>>: lambda "v" $
              Optionals.maybe
                (Flows.fail $ Strings.cat2 (string "No such type in schema: ") (unwrap _Name @@ var "v"))
                (ref inlineTypeDef @@ var "schema")
                (Maps.lookup (var "v") (var "schema"))] @@ var "tr"]
    $ ref rewriteTypeMDef @@ var "f" @@ var "typ"

isFreeVariableInTermDef :: TElement (Name -> Term -> Bool)
isFreeVariableInTermDef = rewritingDefinition "isFreeVariableInTerm" $
 doc "Check whether a variable is free (not bound) in a term" $
 lambda "v" $ lambda "term" $
   Logic.not $ Sets.member (var "v") (ref freeVariablesInTermDef @@ var "term")

isLambdaDef :: TElement (Term -> Bool)
isLambdaDef = rewritingDefinition "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  lambda "term" $ (match _Term (Just false) [
      _Term_function>>: match _Function (Just false) [
        _Function_lambda>>: constant true],
      _Term_let>>: lambda "lt" (ref isLambdaDef @@ (project _Let _Let_environment @@ var "lt"))])
    @@ (ref Strip.fullyStripTermDef @@ var "term")

mapBeneathTypeAnnotationsDef :: TElement ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotationsDef = rewritingDefinition "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  lambda "f" $ lambda "t" $
    match _Type (Just $ var "f" @@ var "t") [
      _Type_annotated>>: lambda "at" $ Core.typeAnnotated $ Core.annotatedType
        (ref mapBeneathTypeAnnotationsDef @@ var "f" @@ (Core.annotatedTypeSubject $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at")] @@ var "t"

normalizeTypeVariablesInTermDef :: TElement (Term -> Term)
normalizeTypeVariablesInTermDef = rewritingDefinition "normalizeTypeVariablesInTerm" $
  doc "Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..." $
  lambda "term" $ lets [
    "substType">: lambdas ["subst", "typ"] $ lets [
      "rewrite">: lambdas ["recurse", "typ"] $
        match _Type (Just $ var "recurse" @@ var "typ") [
          _Type_variable>>: lambda "v" $ Core.typeVariable $ var "replaceName" @@ var "subst" @@ var "v"] @@ var "typ"]
      $ ref rewriteTypeDef @@ var "rewrite" @@ var "typ",
    "replaceName">: lambdas ["subst", "v"] $ Optionals.fromMaybe (var "v") $ Maps.lookup (var "v") (var "subst"),
    "rewriteWithSubst">: lambda "substAndBound" $ lets [
      "subst">: first $ var "substAndBound",
      "boundVars">: second $ var "substAndBound",
      "rewrite">: lambdas ["recurse", "term"] $
        match _Term (Just $ var "recurse" @@ var "term") [
          _Term_function>>: match _Function (Just $ var "recurse" @@ var "term") [
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
                    Math.rangeInt32 (int32 0) (Math.add (var "varsLen") (var "boundVarsLen")),
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
            $ Core.termLet $ Core.letExpression
              (Lists.map (var "rewriteBinding") (var "bindings"))
              (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ var "env"),
          _Term_typeAbstraction>>: lambda "ta" $ Core.termTypeAbstraction $ Core.typeAbstraction
            (var "replaceName" @@ var "subst" @@ (Core.typeAbstractionParameter $ var "ta"))
            (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ (Core.typeAbstractionBody $ var "ta")),
          _Term_typeApplication>>: lambda "tt" $ Core.termTypeApplication $ Core.typedTerm
            (var "rewriteWithSubst" @@ (pair (var "subst") (var "boundVars")) @@ (Core.typedTermTerm $ var "tt"))
            (var "substType" @@ var "subst" @@ (Core.typedTermType $ var "tt"))] @@ var "term"]
      $ ref rewriteTermDef @@ var "rewrite"]
    $ var "rewriteWithSubst" @@ (pair Maps.empty Sets.empty) @@ var "term"

removeTermAnnotationsDef :: TElement (Term -> Term)
removeTermAnnotationsDef = rewritingDefinition "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  lambda "term" $ lets [
    "remove">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"]
      $ match _Term (Just $ var "rewritten") [
        _Term_annotated>>: lambda "at" $ Core.annotatedTermSubject $ var "at",
        _Term_typed>>: lambda "tt" $ Core.typedTermTerm $ var "tt"] @@ var "rewritten"]
    $ ref rewriteTermDef @@ var "remove" @@ var "term"

removeTypeAnnotationsDef :: TElement (Type -> Type)
removeTypeAnnotationsDef = rewritingDefinition "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  lambda "typ" $ lets [
    "remove">: lambdas ["recurse", "typ"] $ lets [
      "rewritten">: var "recurse" @@ var "typ"]
      $ match _Type (Just $ var "rewritten") [
        _Type_annotated>>: lambda "at" $ Core.annotatedTypeSubject $ var "at"] @@ var "rewritten"]
    $ ref rewriteTypeDef @@ var "remove" @@ var "typ"

replaceFreeNameDef :: TElement (Name -> Type -> Type -> Type)
replaceFreeNameDef = rewritingDefinition "replaceFreeName" $
  doc "Replace free occurrences of a name in a type" $
  lambdas ["v", "rep", "typ"] $ lets [
    "mapExpr">: lambdas ["recurse", "t"] $
      match _Type (Just $ var "recurse" @@ var "t") [
        _Type_forall>>: lambda "ft" $ Logic.ifElse
          (Equality.equal (var "v") (Core.forallTypeParameter $ var "ft"))
          (var "t")
          (Core.typeForall $ Core.forallType
            (Core.forallTypeParameter $ var "ft")
            (var "recurse" @@ (Core.forallTypeBody $ var "ft"))),
        _Type_variable>>: lambda "v'" $ Logic.ifElse
          (Equality.equal (var "v") (var "v'"))
          (var "rep")
          (var "t")] @@ var "t"]
    $ ref rewriteTypeDef @@ var "mapExpr" @@ var "typ"

rewriteDef :: TElement (((x -> y) -> x -> y) -> ((x -> y) -> x -> y) -> x -> y)
rewriteDef = rewritingDefinition "rewrite" $ lambdas ["fsub", "f"] $ lets [
  "recurse">: var "f" @@ (var "fsub" @@ var "recurse")] $
  var "recurse"

rewriteTermDef :: TElement (((Term -> Term) -> Term -> Term) -> Term -> Term)
rewriteTermDef = rewritingDefinition "rewriteTerm" $ lambda "f" $ lets [
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
      Core.letExpression
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
      _Term_typed>>: lambda "tt" $ Core.termTypeApplication $ Core.typedTerm
        (var "recurse" @@ (Core.typedTermTerm $ var "tt"))
        (Core.typedTermType $ var "tt"),
      _Term_union>>: lambda "i" $ Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_variable>>: lambda "v" $ Core.termVariable $ var "v"]] $
  ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTermMDef :: TElement (((Term -> Flow s Term) -> Term -> Flow s Term) -> Term -> Flow s Term)
rewriteTermMDef = rewritingDefinition "rewriteTermM" $
  doc "Monadic term rewriting with custom transformation function" $
  lambda "f" $ lets [
    "fsub">: lambda "recurse" $ lambda "term" $ lets [
      "forField">: lambda "f" $
        Flows.map
          (lambda "t" $ Core.fieldWithTerm (var "t") (var "f"))
          (var "recurse" @@ Core.fieldTerm (var "f")),
      "forPair">: lambda "kv" $ lets [
        "k">: first $ var "kv",
        "v">: second $ var "kv"]
        $ Flows.bind (var "recurse" @@ var "k") $
          lambda "km" $
            Flows.bind (var "recurse" @@ var "v") $
              lambda "vm" $ Flows.pure $ pair (var "km") (var "vm"),
      "mapBinding">: lambda "binding" $ lets [
        "k">: Core.letBindingName $ var "binding",
        "v">: Core.letBindingTerm $ var "binding",
        "t">: Core.letBindingType $ var "binding"]
        $ Flows.bind (var "recurse" @@ var "v") $
          lambda "v'" $ Flows.pure $ Core.letBinding (var "k") (var "v'") (var "t")]
      $ match _Term Nothing [
        _Term_annotated>>: lambda "at" $
          Flows.bind (var "recurse" @@ Core.annotatedTermSubject (var "at")) $
            lambda "ex" $ Flows.pure $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
        _Term_application>>: lambda "app" $
          Flows.bind (var "recurse" @@ Core.applicationFunction (var "app")) $
            lambda "lhs" $
              Flows.bind (var "recurse" @@ Core.applicationArgument (var "app")) $
                lambda "rhs" $ Flows.pure $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
        _Term_function>>: lambda "fun" $
          Flows.bind (match _Function Nothing [
            _Function_elimination>>: lambda "e" $
              match _Elimination Nothing [
                _Elimination_product>>: lambda "tp" $ Flows.pure $ Core.functionElimination $ Core.eliminationProduct $ var "tp",
                _Elimination_record>>: lambda "p" $ Flows.pure $ Core.functionElimination $ Core.eliminationRecord $ var "p",
                _Elimination_union>>: lambda "cs" $ lets [
                  "n">: Core.caseStatementTypeName $ var "cs",
                  "def">: Core.caseStatementDefault $ var "cs",
                  "cases">: Core.caseStatementCases $ var "cs"]
                  $ Flows.bind
                      (Optionals.maybe (Flows.pure nothing)
                        (lambda "t" $ Flows.map (unaryFunction just) $ var "recurse" @@ var "t")
                        (var "def")) $
                    lambda "rdef" $
                      Flows.map
                        (lambda "rcases" $ Core.functionElimination $ Core.eliminationUnion $
                          Core.caseStatement (var "n") (var "rdef") (var "rcases"))
                        (Flows.mapList (var "forField") (var "cases")),
                _Elimination_wrap>>: lambda "name" $ Flows.pure $ Core.functionElimination $ Core.eliminationWrap $ var "name"]
              @@ var "e",
            _Function_lambda>>: lambda "l" $ lets [
              "v">: Core.lambdaParameter $ var "l",
              "d">: Core.lambdaDomain $ var "l",
              "body">: Core.lambdaBody $ var "l"]
              $ Flows.bind (var "recurse" @@ var "body") $
                lambda "rbody" $ Flows.pure $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
            _Function_primitive>>: lambda "name" $ Flows.pure $ Core.functionPrimitive $ var "name"]
          @@ var "fun") $
          lambda "rfun" $ Flows.pure $ Core.termFunction $ var "rfun",
        _Term_let>>: lambda "lt" $ lets [
          "bindings">: Core.letBindings $ var "lt",
          "env">: Core.letEnvironment $ var "lt"]
          $ Flows.bind (Flows.mapList (var "mapBinding") (var "bindings")) $
            lambda "rbindings" $
              Flows.bind (var "recurse" @@ var "env") $
                lambda "renv" $ Flows.pure $ Core.termLet $ Core.letExpression (var "rbindings") (var "renv"),
        _Term_list>>: lambda "els" $
          Flows.bind (Flows.mapList (var "recurse") (var "els")) $
            lambda "rels" $ Flows.pure $ Core.termList $ var "rels",
        _Term_literal>>: lambda "v" $ Flows.pure $ Core.termLiteral $ var "v",
        _Term_map>>: lambda "m" $
          Flows.bind (Flows.mapList (var "forPair") $ Maps.toList $ var "m") $
            lambda "pairs" $ Flows.pure $ Core.termMap $ Maps.fromList $ var "pairs",
        _Term_optional>>: lambda "m" $
          Flows.bind (Flows.traverseOptional (var "recurse") (var "m")) $
            lambda "rm" $ Flows.pure $ Core.termOptional $ var "rm",
        _Term_product>>: lambda "tuple" $
          Flows.map
            (lambda "rtuple" $ Core.termProduct $ var "rtuple")
            (Flows.mapList (var "recurse") (var "tuple")),
        _Term_record>>: lambda "r" $ lets [
          "n">: Core.recordTypeName $ var "r",
          "fields">: Core.recordFields $ var "r"]
          $ Flows.map
            (lambda "rfields" $ Core.termRecord $ Core.record (var "n") (var "rfields"))
            (Flows.mapList (var "forField") (var "fields")),
        _Term_set>>: lambda "s" $
          Flows.bind (Flows.mapList (var "recurse") $ Sets.toList $ var "s") $
            lambda "rlist" $ Flows.pure $ Core.termSet $ Sets.fromList $ var "rlist",
        _Term_sum>>: lambda "sum" $ lets [
          "i">: Core.sumIndex $ var "sum",
          "s">: Core.sumSize $ var "sum",
          "trm">: Core.sumTerm $ var "sum"]
          $ Flows.bind (var "recurse" @@ var "trm") $
            lambda "rtrm" $ Flows.pure $ Core.termSum $ Core.sum (var "i") (var "s") (var "rtrm"),
        _Term_typed>>: lambda "tt" $ lets [
          "term1">: Core.typedTermTerm $ var "tt",
          "type2">: Core.typedTermType $ var "tt"]
          $ Flows.bind (var "recurse" @@ var "term1") $
            lambda "rterm1" $ Flows.pure $ Core.termTyped $ Core.typedTerm (var "rterm1") (var "type2"),
        _Term_union>>: lambda "i" $ lets [
          "n">: Core.injectionTypeName $ var "i",
          "field">: Core.injectionField $ var "i"]
          $ Flows.map
            (lambda "rfield" $ Core.termUnion $ Core.injection (var "n") (var "rfield"))
            (var "forField" @@ var "field"),
        _Term_variable>>: lambda "v" $ Flows.pure $ Core.termVariable $ var "v",
        _Term_wrap>>: lambda "wt" $ lets [
          "name">: Core.wrappedTermTypeName $ var "wt",
          "t">: Core.wrappedTermObject $ var "wt"]
          $ Flows.bind (var "recurse" @@ var "t") $
            lambda "rt" $ Flows.pure $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]
      @@ var "term"]
    $ ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTermMetaDef :: TElement ((M.Map Name Term -> M.Map Name Term) -> Term -> Term)
rewriteTermMetaDef = rewritingDefinition "rewriteTermMeta" $
  doc "Rewrite term metadata/annotations" $
  lambda "mapping" $ lets [
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"]
      $ match _Term (Just $ var "rewritten") [
        _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
          (Core.annotatedTermSubject $ var "at")
          (var "mapping" @@ (Core.annotatedTermAnnotation $ var "at"))] @@ var "rewritten"]
    $ ref rewriteTermDef @@ var "rewrite"

rewriteTermMetaMDef :: TElement ((M.Map Name Term -> Flow s (M.Map Name Term)) -> Term -> Flow s Term)
rewriteTermMetaMDef = rewritingDefinition "rewriteTermMetaM" $
  doc "Monadic rewrite of term metadata/annotations" $
  lambda "mapping" $ lets [
    "rewrite">: lambdas ["recurse", "term"] $
      Flows.bind (var "recurse" @@ var "term") $
        lambda "r" $
          match _Term (Just $ Flows.pure $ var "r") [
            _Term_annotated>>: lambda "at" $
              Flows.bind (var "mapping" @@ (Core.annotatedTermAnnotation $ var "at")) $
                lambda "newAnn" $ Flows.pure $ Core.termAnnotated $ Core.annotatedTerm
                  (Core.annotatedTermSubject $ var "at")
                  (var "newAnn")] @@ var "r"]
    $ ref rewriteTermMDef @@ var "rewrite"

rewriteTypeDef :: TElement (((Type -> Type) -> Type -> Type) -> Type -> Type)
rewriteTypeDef = rewritingDefinition "rewriteType" $ lambda "f" $ lets [
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
      _Type_variable>>: lambda "v" $ Core.typeVariable $ var "v",
      _Type_wrap>>: lambda "wt" $ Core.typeWrap $ Core.wrappedType
        (Core.wrappedTypeTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTypeObject $ var "wt"))]] $
  ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTypeMDef :: TElement (((Type -> Flow s Type) -> Type -> Flow s Type) -> Type -> Flow s Type)
rewriteTypeMDef = rewritingDefinition "rewriteTypeM" $
  doc "Monadic type rewriting" $
  lambda "f" $ lets [
    "fsub">: lambda "recurse" $ lambda "typ" $
      match _Type Nothing [
        _Type_annotated>>: lambda "at" $
          Flows.bind (var "recurse" @@ (Core.annotatedTypeSubject $ var "at")) $
            lambda "t" $ Flows.pure $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTypeAnnotation $ var "at"),
        _Type_application>>: lambda "at" $
          Flows.bind (var "recurse" @@ (Core.applicationTypeFunction $ var "at")) $
            lambda "lhs" $
              Flows.bind (var "recurse" @@ (Core.applicationTypeArgument $ var "at")) $
                lambda "rhs" $ Flows.pure $ Core.typeApplication $ Core.applicationType (var "lhs") (var "rhs"),
        _Type_function>>: lambda "ft" $
          Flows.bind (var "recurse" @@ (Core.functionTypeDomain $ var "ft")) $
            lambda "dom" $
              Flows.bind (var "recurse" @@ (Core.functionTypeCodomain $ var "ft")) $
                lambda "cod" $ Flows.pure $ Core.typeFunction $ Core.functionType (var "dom") (var "cod"),
        _Type_forall>>: lambda "ft" $
          Flows.bind (var "recurse" @@ (Core.forallTypeBody $ var "ft")) $
            lambda "b" $ Flows.pure $ Core.typeForall $ Core.forallType (Core.forallTypeParameter $ var "ft") (var "b"),
        _Type_list>>: lambda "t" $
          Flows.bind (var "recurse" @@ var "t") $
            lambda "rt" $ Flows.pure $ Core.typeList $ var "rt",
        _Type_literal>>: lambda "lt" $ Flows.pure $ Core.typeLiteral $ var "lt",
        _Type_map>>: lambda "mt" $
          Flows.bind (var "recurse" @@ (Core.mapTypeKeys $ var "mt")) $
            lambda "kt" $
              Flows.bind (var "recurse" @@ (Core.mapTypeValues $ var "mt")) $
                lambda "vt" $ Flows.pure $ Core.typeMap $ Core.mapType (var "kt") (var "vt"),
        _Type_optional>>: lambda "t" $
          Flows.bind (var "recurse" @@ var "t") $
            lambda "rt" $ Flows.pure $ Core.typeOptional $ var "rt",
        _Type_product>>: lambda "types" $
          Flows.bind (Flows.mapList (var "recurse") (var "types")) $
            lambda "rtypes" $ Flows.pure $ Core.typeProduct $ var "rtypes",
        _Type_record>>: lambda "rt" $ lets [
          "name">: Core.rowTypeTypeName $ var "rt",
          "fields">: Core.rowTypeFields $ var "rt",
          "forField">: lambda "f" $
            Flows.bind (var "recurse" @@ (Core.fieldTypeType $ var "f")) $
              lambda "t" $ Flows.pure $ Core.fieldTypeWithType (var "f") (var "t")]
          $ Flows.bind (Flows.mapList (var "forField") (var "fields")) $
            lambda "rfields" $ Flows.pure $ Core.typeRecord $ Core.rowType (var "name") (var "rfields"),
        _Type_set>>: lambda "t" $
          Flows.bind (var "recurse" @@ var "t") $
            lambda "rt" $ Flows.pure $ Core.typeSet $ var "rt",
        _Type_sum>>: lambda "types" $
          Flows.bind (Flows.mapList (var "recurse") (var "types")) $
            lambda "rtypes" $ Flows.pure $ Core.typeSum $ var "rtypes",
        _Type_union>>: lambda "rt" $ lets [
          "name">: Core.rowTypeTypeName $ var "rt",
          "fields">: Core.rowTypeFields $ var "rt",
          "forField">: lambda "f" $
            Flows.bind (var "recurse" @@ (Core.fieldTypeType $ var "f")) $
              lambda "t" $ Flows.pure $ Core.fieldTypeWithType (var "f") (var "t")]
          $ Flows.bind (Flows.mapList (var "forField") (var "fields")) $
            lambda "rfields" $ Flows.pure $ Core.typeUnion $ Core.rowType (var "name") (var "rfields"),
        _Type_variable>>: lambda "v" $ Flows.pure $ Core.typeVariable $ var "v",
        _Type_wrap>>: lambda "wt" $
          Flows.bind (var "recurse" @@ (Core.wrappedTypeObject $ var "wt")) $
            lambda "t" $ Flows.pure $ Core.typeWrap $ Core.wrappedType (Core.wrappedTypeTypeName $ var "wt") (var "t")]
      @@ var "typ"]
    $ ref rewriteDef @@ var "fsub" @@ var "f"

rewriteTypeMetaDef :: TElement ((M.Map Name Term -> M.Map Name Term) -> Type -> Type)
rewriteTypeMetaDef = rewritingDefinition "rewriteTypeMeta" $
  doc "Rewrite type metadata/annotations" $
  lambda "mapping" $ lets [
    "rewrite">: lambdas ["recurse", "typ"] $ lets [
      "rewritten">: var "recurse" @@ var "typ"]
      $ match _Type (Just $ var "rewritten") [
        _Type_annotated>>: lambda "at" $ Core.typeAnnotated $ Core.annotatedType
          (Core.annotatedTypeSubject $ var "at")
          (var "mapping" @@ (Core.annotatedTypeAnnotation $ var "at"))] @@ var "rewritten"]
    $ ref rewriteTypeDef @@ var "rewrite"

simplifyTermDef :: TElement (Term -> Term)
simplifyTermDef = rewritingDefinition "simplifyTerm" $
  doc "Simplify terms by applying beta reduction where possible" $
  lambda "term" $ lets [
    "simplify">: lambdas ["recurse", "term"] $ lets [
      "stripped">: ref Strip.fullyStripTermDef @@ var "term"]
      $ var "recurse" @@ (cases _Term (var "stripped") (Just $ var "term") [
        _Term_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app",
          "strippedLhs">: ref Strip.fullyStripTermDef @@ var "lhs"]
          $ cases _Term (var "strippedLhs") (Just $ var "term") [
            _Term_function>>: match _Function (Just $ var "term") [
              _Function_lambda>>: lambda "l" $ lets [
                "var">: Core.lambdaParameter $ var "l",
                "body">: Core.lambdaBody $ var "l"]
                $ Logic.ifElse (Sets.member (var "var") (ref freeVariablesInTermDef @@ var "body"))
                  (lets [
                    "strippedRhs">: ref Strip.fullyStripTermDef @@ var "rhs"]
                    $ match _Term (Just $ var "term") [
                      _Term_variable>>: lambda "v" $ ref simplifyTermDef @@ (ref substituteVariableDef @@ var "var" @@ var "v" @@ var "body")] @@ var "strippedRhs")
                  (ref simplifyTermDef @@ var "body")]]])]
    $ ref rewriteTermDef @@ var "simplify" @@ var "term"

stripTermRecursiveDef :: TElement (Term -> Term)
stripTermRecursiveDef = rewritingDefinition "stripTermRecursive" $
  doc "Recursively strip all annotations from a term" $
  lambda "term" $ lets [
    "strip">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term"]
      $ match _Term (Just $ var "rewritten") [
        _Term_annotated>>: lambda "at" $ Core.annotatedTermSubject $ var "at",
        _Term_typed>>: lambda "tt" $ Core.typedTermTerm $ var "tt"] @@ var "rewritten"]
    $ ref rewriteTermDef @@ var "strip" @@ var "term"

stripTypeRecursiveDef :: TElement (Type -> Type)
stripTypeRecursiveDef = rewritingDefinition "stripTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  lambda "typ" $ lets [
    "strip">: lambdas ["recurse", "typ"] $ lets [
      "rewritten">: var "recurse" @@ var "typ"]
      $ match _Type (Just $ var "rewritten") [
        _Type_annotated>>: lambda "at" $ Core.annotatedTypeSubject $ var "at"] @@ var "rewritten"]
    $ ref rewriteTypeDef @@ var "strip" @@ var "typ"

stripTypeSchemeRecursiveDef :: TElement (TypeScheme -> TypeScheme)
stripTypeSchemeRecursiveDef = rewritingDefinition "stripTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "typ">: Core.typeSchemeType $ var "ts"]
    $ Core.typeScheme (var "vars") (ref stripTypeRecursiveDef @@ var "typ")

stripTypesFromTermDef :: TElement (Term -> Term)
stripTypesFromTermDef = rewritingDefinition "stripTypesFromTerm" $
  doc "Strip type annotations from terms while preserving other annotations" $
  lambda "term" $ lets [
    "strip">: lambdas ["recurse", "term"] $ lets [
      "rewritten">: var "recurse" @@ var "term",
      "stripBinding">: lambda "b" $ Core.letBinding
        (Core.letBindingName $ var "b")
        (Core.letBindingTerm $ var "b")
        nothing]
      $ cases _Term (var "rewritten") (Just $ var "rewritten") [
        _Term_function>>: lambda "f" $ cases _Function (var "f") (Just $ Core.termFunction $ var "f") [
          _Function_elimination>>: lambda "e" $ cases _Elimination (var "e") (Just $ Core.termFunction $ Core.functionElimination $ var "e") [
            _Elimination_product>>: lambda "tp" $ Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $
              Core.tupleProjection
                (Core.tupleProjectionIndex $ var "tp")
                (Core.tupleProjectionArity $ var "tp")
                nothing],
          _Function_lambda>>: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            nothing
            (Core.lambdaBody $ var "l")],
        _Term_let>>: lambda "lt" $ Core.termLet $ Core.letExpression
          (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
          (Core.letEnvironment $ var "lt"),
        _Term_typeAbstraction>>: lambda "ta" $ Core.typeAbstractionBody $ var "ta",
        _Term_typeApplication>>: lambda "tt" $ Core.typedTermTerm $ var "tt",
        _Term_typed>>: lambda "tt" $ Core.typedTermTerm $ var "tt"]]
    $ ref rewriteTermDef @@ var "strip" @@ var "term"

substituteTypeVariablesDef :: TElement (M.Map Name Name -> Type -> Type)
substituteTypeVariablesDef = rewritingDefinition "substituteTypeVariables" $
  doc "Substitute type variables in a type" $
  lambdas ["subst", "typ"] $ lets [
    "replace">: lambdas ["recurse", "typ"] $
      match _Type (Just $ var "recurse" @@ var "typ") [
        _Type_variable>>: lambda "n" $ Core.typeVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst")] @@ var "typ"]
    $ ref rewriteTypeDef @@ var "replace" @@ var "typ"

substituteVariableDef :: TElement (Name -> Name -> Term -> Term)
substituteVariableDef = rewritingDefinition "substituteVariable" $
  doc "Substitute one variable for another in a term" $
  lambdas ["from", "to", "term"] $ lets [
    "replace">: lambdas ["recurse", "term"] $
      cases _Term (var "term") (Just $ var "recurse" @@ var "term") [
        _Term_variable>>: lambda "x" $ Core.termVariable $ Logic.ifElse (Equality.equal (var "x") (var "from")) (var "to") (var "x"),
        _Term_function>>: match _Function (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: lambda "l" $ Logic.ifElse
            (Equality.equal (Core.lambdaParameter $ var "l") (var "from"))
            (var "term")
            (var "recurse" @@ var "term")]]]
    $ ref rewriteTermDef @@ var "replace" @@ var "term"

substituteVariablesDef :: TElement (M.Map Name Name -> Term -> Term)
substituteVariablesDef = rewritingDefinition "substituteVariables" $
  doc "Substitute multiple variables in a term" $
  lambdas ["subst", "term"] $ lets [
    "replace">: lambdas ["recurse", "term"] $
      cases _Term (var "term") (Just $ var "recurse" @@ var "term") [
        _Term_variable>>: lambda "n" $ Core.termVariable $ Optionals.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst"),
        _Term_function>>: match _Function (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: lambda "l" $
            Optionals.maybe
              (var "recurse" @@ var "term")
              (constant $ var "term")
              (Maps.lookup (Core.lambdaParameter $ var "l") (var "subst"))]]]
    $ ref rewriteTermDef @@ var "replace" @@ var "term"

subtermsDef :: TElement (Term -> [Term])
subtermsDef = rewritingDefinition "subterms" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ list [Core.annotatedTermSubject $ var "at"],
    _Term_application>>: lambda "p" $ list [
      Core.applicationFunction $ var "p",
      Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function (Just $ list []) [
        _Function_elimination>>: match _Elimination (Just $ list []) [
            _Elimination_union>>: lambda "cs" $ Lists.concat2
              ((primitive _optionals_maybe @@ (list []) @@ (lambda "t" $ list [var "t"])) @@ (Core.caseStatementDefault $ var "cs"))
              (Lists.map (unaryFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs"))],
        _Function_lambda>>: lambda "l" $ list [Core.lambdaBody $ var "l"]],
    _Term_let>>: lambda "lt" $ Lists.cons
      (Core.letEnvironment $ var "lt")
      (Lists.map (unaryFunction Core.letBindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: lambda "l" $ var "l",
    _Term_literal>>: constant $ list [],
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map (lambda "p" $ list [first $ var "p", second $ var "p"]) (Maps.toList $ var "m"))),
    _Term_optional>>: primitive _optionals_maybe @@  (list []) @@ (lambda "t" $ list [var "t"]),
    _Term_product>>: lambda "tuple" $ var "tuple",
    _Term_record>>: lambda "rt" (Lists.map (unaryFunction Core.fieldTerm) (Core.recordFields $ var "rt")),
    _Term_set>>: lambda "l" $ Sets.toList $ var "l",
    _Term_sum>>: lambda "st" $ list [Core.sumTerm $ var "st"],
    _Term_typeAbstraction>>: lambda "ta" $ list [Core.typeAbstractionBody $ var "ta"],
    _Term_typeApplication>>: lambda "ta" $ list [Core.typedTermTerm $ var "ta"],
    _Term_typed>>: lambda "tt" $ list [Core.typedTermTerm $ var "tt"],
    _Term_union>>: lambda "ut" $ list [Core.fieldTerm $ (Core.injectionField $ var "ut")],
    _Term_variable>>: constant $ list [],
    _Term_wrap>>: lambda "n" $ list [Core.wrappedTermObject $ var "n"]]

subtermsWithAccessorsDef :: TElement (Term -> [(TermAccessor, Term)])
subtermsWithAccessorsDef = rewritingDefinition "subtermsWithAccessors" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ single Mantle.termAccessorAnnotatedSubject $ Core.annotatedTermSubject $ var "at",
    _Term_application>>: lambda "p" $ list [
      result Mantle.termAccessorApplicationFunction $ Core.applicationFunction $ var "p",
      result Mantle.termAccessorApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function (Just none) [
        _Function_elimination>>: match _Elimination (Just none) [
            _Elimination_union>>: lambda "cs" $ Lists.concat2
              ((primitive _optionals_maybe @@  none @@ (lambda "t" $ single Mantle.termAccessorUnionCasesDefault $ var "t"))
                @@ (Core.caseStatementDefault $ var "cs"))
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
    _Term_optional>>: primitive _optionals_maybe @@  none @@ (lambda "t" $ single Mantle.termAccessorOptionalTerm $ var "t"),
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
    _Term_typed>>: lambda "tt" $
      single Mantle.termAccessorTypedTerm $
      Core.typedTermTerm $ var "tt",
    _Term_union>>: lambda "ut" $
      single Mantle.termAccessorInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_variable>>: constant none,
    _Term_wrap>>: lambda "n" $ single Mantle.termAccessorWrappedTerm $ Core.wrappedTermObject $ var "n"]
  where
    none = list []
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result Mantle.termAccessorAnnotatedSubject term

subtypesDef :: TElement (Type -> [Type])
subtypesDef = rewritingDefinition "subtypes" $
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
    _Type_variable>>: constant $ list [],
    _Type_wrap>>: lambda "nt" $ list [Core.wrappedTypeObject $ var "nt"]]

termDependencyNamesDef :: TElement (Bool -> Bool -> Bool -> Term -> S.Set Name)
termDependencyNamesDef = rewritingDefinition "termDependencyNames" $
  doc "Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that" $
  lambdas ["withVars", "withPrims", "withNoms"] $ lets [
    "addNames">: lambdas ["names", "term"] $ lets [
      "nominal">: lambda "name" $ Logic.ifElse (var "withNoms")
        (Sets.insert (var "name") (var "names"))
        (var "names"),
      "prim">: lambda "name" $ Logic.ifElse (var "withPrims")
        (Sets.insert (var "name") (var "names"))
        (var "names"),
      "var">: lambda "name" $ Logic.ifElse (var "withVars")
        (Sets.insert (var "name") (var "names"))
        (var "names")]
      $ cases _Term (var "term") (Just $ var "names") [
        _Term_function>>: lambda "f" $ cases _Function (var "f") (Just $ var "names") [
          _Function_primitive>>: lambda "name" $ var "prim" @@ var "name",
          _Function_elimination>>: lambda "e" $ cases _Elimination (var "e") (Just $ var "names") [
            _Elimination_record>>: lambda "proj" $ var "nominal" @@ (Core.projectionTypeName $ var "proj"),
            _Elimination_union>>: lambda "caseStmt" $ var "nominal" @@ (Core.caseStatementTypeName $ var "caseStmt"),
            _Elimination_wrap>>: lambda "name" $ var "nominal" @@ var "name"]],
        _Term_record>>: lambda "record" $ var "nominal" @@ (Core.recordTypeName $ var "record"),
        _Term_union>>: lambda "injection" $ var "nominal" @@ (Core.injectionTypeName $ var "injection"),
        _Term_variable>>: lambda "name" $ var "var" @@ var "name",
        _Term_wrap>>: lambda "wrappedTerm" $ var "nominal" @@ (Core.wrappedTermTypeName $ var "wrappedTerm")]]
    $ ref foldOverTermDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty

toShortNamesDef :: TElement ([Name] -> M.Map Name Name)
toShortNamesDef = rewritingDefinition "toShortNames" $
  doc "Generate short names from a list of fully qualified names" $
  lambda "original" $ lets [
    "groupNamesByLocal">: lambda "names" $ Lists.foldl (var "addName") Maps.empty (var "names"),
    "addName">: lambda "acc" $ lambda "name" $ lets [
      "local">: ref Qnames.localNameOfDef @@ var "name",
      "group">: Optionals.fromMaybe Sets.empty $ Maps.lookup (var "local") (var "acc")]
      $ Maps.insert (var "local") (Sets.insert (var "name") (var "group")) (var "acc"),
    "groups">: var "groupNamesByLocal" @@ var "original",
    "renameGroup">: lambda "localNames" $ lets [
      "local">: first $ var "localNames",
      "names">: second $ var "localNames",
      "rangeFrom">: lambda "start" $ Lists.cons (var "start") (var "rangeFrom" @@ (Math.add (var "start") (int32 1))),
      "rename">: lambda "name" $ lambda "i" $ pair (var "name") $ Core.name $
        Logic.ifElse (Equality.gtInt32 (var "i") (int32 1))
          (Strings.cat2 (var "local") (Literals.showInt32 $ var "i"))
          (var "local")]
      $ Lists.zipWith (var "rename") (Sets.toList $ var "names") (var "rangeFrom" @@ int32 1)]
    $ Maps.fromList $ Lists.concat $ Lists.map (var "renameGroup") $ Maps.toList $ var "groups"

topologicalSortBindingsDef :: TElement (M.Map Name Term -> [[(Name, Term)]])
topologicalSortBindingsDef = rewritingDefinition "topologicalSortBindings" $
  doc "Topological sort of connected components, in terms of dependencies between variable/term binding pairs" $
  lambda "bindingMap" $ lets [
    "bindings">: Maps.toList $ var "bindingMap",
    "keys">: Sets.fromList $ Lists.map (unaryFunction first) (var "bindings"),
    "hasTypeAnnotation">: lambda "term" $
      cases _Term (var "term") (Just false) [
        _Term_annotated>>: lambda "at" $ var "hasTypeAnnotation" @@ (Core.annotatedTermSubject $ var "at"),
        _Term_typed>>: constant true],
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
topologicalSortElementsDef = rewritingDefinition "topologicalSortElements" $
  doc "Topological sort of elements based on their dependencies" $
  lambda "els" $ lets [
    "adjlist">: lambda "e" $ pair
      (Graph.elementName $ var "e")
      (Sets.toList $ ref termDependencyNamesDef @@ false @@ true @@ true @@ (Graph.elementTerm $ var "e"))]
    $ ref Sorting.topologicalSortDef @@ Lists.map (var "adjlist") (var "els")

typeDependencyNamesDef :: TElement (Bool -> Bool -> Type -> S.Set Name)
typeDependencyNamesDef = rewritingDefinition "typeDependencyNames" $
  lambdas ["withSchema", "excludeUnit", "typ"] $
    Logic.ifElse (var "withSchema")
      (Sets.union
        (ref freeVariablesInTypeDef @@ var "typ")
        (ref typeNamesInTypeDef @@ var "excludeUnit" @@ var "typ"))
      (ref freeVariablesInTypeDef @@ var "typ")

typeNamesInTypeDef :: TElement (Bool -> Type -> S.Set Name)
typeNamesInTypeDef = rewritingDefinition "typeNamesInType" $
  lambda "excludeUnit" $ lets [
    "addNames">: lambdas ["names", "typ"] $ cases _Type (var "typ") (Just $ var "names") [
      _Type_record>>: lambda "rowType" $ lets [
        "tname">: Core.rowTypeTypeName $ var "rowType"]
        $ Logic.ifElse
          (Logic.or (Logic.not $ var "excludeUnit") (Logic.not $ Core.equalName_ (var "tname") $ Core.nameLift _Unit))
          (Sets.insert (var "tname") (var "names"))
          (var "names"),
      _Type_union>>: lambda "rowType" $ lets [
        "tname">: Core.rowTypeTypeName $ var "rowType"]
        $ Sets.insert (var "tname") (var "names"),
      _Type_wrap>>: lambda "wrappedType" $ lets [
        "tname">: Core.wrappedTypeTypeName $ var "wrappedType"]
        $ Sets.insert (var "tname") (var "names")]]
    $ ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty
