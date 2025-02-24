{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Rewriting where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All

import Hydra.Dsl.Mantle


rewritingDefinition :: String -> TTerm a -> TElement a
rewritingDefinition = definitionInModule hydraRewritingModule

hydraRewritingModule :: Module
hydraRewritingModule = Module (Namespace "hydra.rewriting") elements
    [hydraComputeModule, hydraConstantsModule, hydraStripModule] [hydraCoreModule] $
    Just ("Utilities for type and term rewriting and analysis.")
  where
   elements = [
     el foldOverTermDef,
     el foldOverTypeDef,
     el freeVariablesInTermDef,
     el freeVariablesInTypeDef,
     el isLambdaDef,
     el subtermsDef,
     el subtermsWithAccessorsDef,
     el subtypesDef]

foldOverTermDef :: TElement (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTermDef = rewritingDefinition "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  functionN [TypeVariable _TraversalOrder, tFun tX (tFun termT tX), tX, termT, tX] $
  lambda "order" $ lambda "fld" $ lambda "b0" $ lambda "term" $ (match _TraversalOrder Nothing [
    _TraversalOrder_pre>>: constant (Base.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (ref subtermsDef @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Base.fold (ref foldOverTermDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtermsDef @@ var "term"))
      @@ var "term")] @@ var "order")

foldOverTypeDef :: TElement (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverTypeDef = rewritingDefinition "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  functionN [TypeVariable _TraversalOrder, tFun tX (tFun typeT tX), tX, typeT, tX] $
  lambda "order" $ lambda "fld" $ lambda "b0" $ lambda "typ" $ (match _TraversalOrder Nothing [
    _TraversalOrder_pre>>: constant (Base.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (ref subtypesDef @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Base.fold (ref foldOverTypeDef @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (ref subtypesDef @@ var "typ"))
      @@ var "typ")] @@ var "order")

freeVariablesInTermDef :: TElement (Term -> S.Set Name)
freeVariablesInTermDef = rewritingDefinition "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  function termT (tSet nameT) $
  lambda "term" $ lets [
    "dfltVars">: typed (tSet nameT) $ Base.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (ref freeVariablesInTermDef @@ var "t"))
      @@ Sets.empty
      @@ (ref subtermsDef @@ var "term")]
    $ match _Term (Just $ var "dfltVars") [
      _Term_function>>: match _Function (Just $ var "dfltVars") [
        _Function_lambda>>: lambda "l" (Sets.remove
          (Core.lambdaParameter @@ var "l")
          (ref freeVariablesInTermDef @@ (Core.lambdaBody @@ var "l")))],
--      TODO: restore the following
--      _Term_let>>: lambda "l" (Sets.difference
--        @@ (ref freeVariablesInTermDef @@ (Core.letEnvironment @@ var "l"))
--        @@ (Sets.fromList (Lists.map first (Maps.toList (Core.letBindings @@ var "l"))))),
      _Term_variable>>: lambda "v" (Sets.singleton $ var "v")] @@ var "term"

freeVariablesInTypeDef :: TElement (Type -> S.Set Name)
freeVariablesInTypeDef = rewritingDefinition "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  function typeT (tSet nameT) $
  lambda "typ" $ lets [
    "dfltVars">: typed (tSet nameT) $ Base.fold (lambda "s" $ lambda "t" $ Sets.union (var "s") (recurse @@ var "t"))
      @@ Sets.empty
      @@ (ref subtypesDef @@ var "typ")]
    $ match _Type (Just $ var "dfltVars") [
      _Type_lambda>>: lambda "lt" (Sets.remove
          (Core.lambdaTypeParameter @@ var "lt")
          (recurse @@ (Core.lambdaTypeBody @@ var "lt"))),
      -- TODO: let-types
      _Type_variable>>: lambda "v" (Sets.singleton $ var "v")] @@ var "typ"
  where
    recurse = ref freeVariablesInTypeDef

isLambdaDef :: TElement (Term -> Bool)
isLambdaDef = rewritingDefinition "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  function termT Types.boolean $
  lambda "term" $ (match _Term (Just false) [
      _Term_function>>: match _Function (Just false) [
        _Function_lambda>>: constant true],
      _Term_let>>: lambda "lt" (ref isLambdaDef @@ (project _Let _Let_environment @@ var "lt"))])
    @@ (ref fullyStripTermDef @@ var "term")

subtermsDef :: TElement (Term -> [Term])
subtermsDef = rewritingDefinition "subterms" $
  doc "Find the children of a given term" $
  function termT (tList termT) $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ list [Core.annotatedTermSubject @@ var "at"],
    _Term_application>>: lambda "p" $ list [
      Core.applicationFunction @@ var "p",
      Core.applicationArgument @@ var "p"],
    _Term_function>>: match _Function (Just $ list []) [
        _Function_elimination>>: match _Elimination (Just $ list []) [
            _Elimination_list>>: lambda "fld" $ list [var "fld"],
            _Elimination_optional>>: lambda "oc" $ list [
              Core.optionalCasesNothing @@ var "oc",
              Core.optionalCasesJust @@ var "oc"],
            _Elimination_union>>: lambda "cs" $ Lists.concat2
              ((matchOpt (list []) (lambda "t" $ list [var "t"])) @@ (Core.caseStatementDefault @@ var "cs"))
              (Lists.map Core.fieldTerm (Core.caseStatementCases @@ var "cs"))],
        _Function_lambda>>: lambda "l" $ list [Core.lambdaBody @@ var "l"]],
    _Term_let>>: lambda "lt" $ Lists.cons
      (Core.letEnvironment @@ var "lt")
      (Lists.map Core.letBindingTerm (Core.letBindings @@ var "lt")),
    _Term_list>>: lambda "l" $ var "l",
    _Term_literal>>: constant $ list [],
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map (lambda "p" $ list [first @@ var "p", second @@ var "p"]) (Maps.toList $ var "m"))),
    _Term_optional>>: matchOpt (list []) (lambda "t" $ list [var "t"]),
    _Term_product>>: lambda "tuple" $ var "tuple",
    _Term_record>>: lambda "rt" (Lists.map Core.fieldTerm (Core.recordFields @@ var "rt")),
    _Term_set>>: lambda "l" $ Sets.toList $ var "l",
    _Term_sum>>: lambda "st" $ list [Core.sumTerm @@ var "st"],
    _Term_typeAbstraction>>: lambda "ta" $ list [Core.typeAbstractionBody @@ var "ta"],
    _Term_typeApplication>>: lambda "ta" $ list [Core.typedTermTerm @@ var "ta"],
    _Term_typed>>: lambda "tt" $ list [Core.typedTermTerm @@ var "tt"],
    _Term_union>>: lambda "ut" $ list [Core.fieldTerm @@ (Core.injectionField @@ var "ut")],
    _Term_variable>>: constant $ list [],
    _Term_wrap>>: lambda "n" $ list [Core.wrappedTermObject @@ var "n"]]

subtermsWithAccessorsDef :: TElement (Term -> [(TermAccessor, Term)])
subtermsWithAccessorsDef = rewritingDefinition "subtermsWithAccessors" $
  doc "Find the children of a given term" $
  function termT (tList $ tPair termAccessorT termT) $
  match _Term Nothing [
    _Term_annotated>>: lambda "at" $ single termAccessorAnnotatedSubject $ Core.annotatedTermSubject @@ var "at",
    _Term_application>>: lambda "p" $ list [
      result termAccessorApplicationFunction $ Core.applicationFunction @@ var "p",
      result termAccessorApplicationArgument $ Core.applicationArgument @@ var "p"],
    _Term_function>>: match _Function (Just none) [
        _Function_elimination>>: match _Elimination (Just none) [
            _Elimination_list>>: lambda "fld" $ single termAccessorListFold $ var "fld",
            _Elimination_optional>>: lambda "oc" $ list [
              result termAccessorOptionalCasesNothing $ Core.optionalCasesNothing @@ var "oc",
              result termAccessorOptionalCasesJust $ Core.optionalCasesJust @@ var "oc"],
            _Elimination_union>>: lambda "cs" $ Lists.concat2
              ((matchOpt none (lambda "t" $ single termAccessorUnionCasesDefault $ var "t")) @@ (Core.caseStatementDefault @@ var "cs"))
              (Lists.map
                (lambda "f" $ result (termAccessorUnionCasesBranch $ Core.fieldName @@ var "f") $ Core.fieldTerm @@ var "f")
                (Core.caseStatementCases @@ var "cs"))],
        _Function_lambda>>: lambda "l" $ single termAccessorLambdaBody $ Core.lambdaBody @@ var "l"],
    _Term_let>>: lambda "lt" $ Lists.cons
      (result termAccessorLetEnvironment $ Core.letEnvironment @@ var "lt")
      (Lists.map
        (lambda "b" $ result (termAccessorLetBinding $ Core.letBindingName @@ var "b") $ Core.letBindingTerm @@ var "b")
        (Core.letBindings @@ var "lt")),
    _Term_list>>: lambda "l" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map
        (lambda "p" $ list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (termAccessorMapKey $ int32 0) $ first @@ var "p",
          result (termAccessorMapValue $ int32 0) $ second @@ var "p"])
        (Maps.toList $ var "m"))),
    _Term_optional>>: matchOpt none (lambda "t" $ single termAccessorOptionalTerm $ var "t"),
    _Term_product>>: lambda "p" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorProductTerm $ int32 0) $ var "e")
      (var "p"),
    _Term_record>>: lambda "rt" (Lists.map
      (lambda "f" $ result (termAccessorRecordField $ Core.fieldName @@ var "f") $ Core.fieldTerm @@ var "f")
      (Core.recordFields @@ var "rt")),
    _Term_set>>: lambda "s" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_sum>>: lambda "st" $
      single termAccessorSumTerm $
      Core.sumTerm @@ var "st",
    _Term_typeAbstraction>>: lambda "ta" $
      single termAccessorTypeAbstractionBody $
      Core.typeAbstractionBody @@ var "ta",
    _Term_typeApplication>>: lambda "ta" $
      single termAccessorTypeApplicationTerm $
      Core.typedTermTerm @@ var "ta",
    _Term_typed>>: lambda "tt" $
      single termAccessorTypedTerm $
      Core.typedTermTerm @@ var "tt",
    _Term_union>>: lambda "ut" $
      single termAccessorInjectionTerm $
      Core.fieldTerm @@ (Core.injectionField @@ var "ut"),
    _Term_variable>>: constant none,
    _Term_wrap>>: lambda "n" $ single termAccessorWrappedTerm $ Core.wrappedTermObject @@ var "n"]
  where
    none = list []
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result termAccessorAnnotatedSubject term

subtypesDef :: TElement (Type -> [Type])
subtypesDef = rewritingDefinition "subtypes" $
  doc "Find the children of a given type expression" $
  function typeT (tList typeT) $
  match _Type Nothing [
    _Type_annotated>>: lambda "at" $ list [Core.annotatedTypeSubject @@ var "at"],
    _Type_application>>: lambda "at" $ list [
      Core.applicationTypeFunction @@ var "at",
      Core.applicationTypeArgument @@ var "at"],
    _Type_function>>: lambda "ft" $ list [
      Core.functionTypeDomain @@ var "ft",
      Core.functionTypeCodomain @@ var "ft"],
    _Type_lambda>>: lambda "lt" $ list [Core.lambdaTypeBody @@ var "lt"],
    _Type_list>>: lambda "lt" $ list [var "lt"],
    _Type_literal>>: constant $ list [],
    _Type_map>>: lambda "mt" $ list [
      Core.mapTypeKeys @@ var "mt",
      Core.mapTypeValues @@ var "mt"],
    _Type_optional>>: lambda "ot" $ list [var "ot"],
    _Type_product>>: lambda "pt" $ var "pt",
    _Type_record>>: lambda "rt" (Lists.map Core.fieldTypeType (Core.rowTypeFields @@ var "rt")),
    _Type_set>>: lambda "st" $ list [var "st"],
    _Type_sum>>: lambda "st" $ var "st",
    _Type_union>>: lambda "rt" (Lists.map Core.fieldTypeType (Core.rowTypeFields @@ var "rt")),
    _Type_variable>>: constant $ list [],
    _Type_wrap>>: lambda "nt" $ list [Core.wrappedTypeObject @@ var "nt"]]
