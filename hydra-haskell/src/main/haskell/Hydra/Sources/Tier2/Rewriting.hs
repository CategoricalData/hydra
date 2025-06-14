{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Rewriting where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Coders          as Coders
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.TTerms          as TTerms
import qualified Hydra.Dsl.TTypes          as TTypes
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import Hydra.Dsl.Mantle
import Hydra.Sources.Libraries


rewritingDefinition :: String -> TTerm a -> TElement a
rewritingDefinition = definitionInModule hydraRewritingModule

hydraRewritingModule :: Module
hydraRewritingModule = Module (Namespace "hydra.rewriting") elements
    [hydraComputeModule, hydraConstantsModule, hydraStripModule] [hydraCodersModule, hydraMantleModule] $
    Just ("Utilities for type and term rewriting and analysis.")
  where
   elements = [
     el foldOverTermDef,
     el foldOverTypeDef,
     el freeVariablesInTermDef,
     el freeVariablesInTypeDef,
     el isFreeVariableInTermDef,
     el isLambdaDef,
     el mapBeneathTypeAnnotationsDef,
     el rewriteDef,
     el rewriteTermDef,
     el rewriteTypeDef,
     el subtermsDef,
     el subtermsWithAccessorsDef,
     el subtypesDef,
     el termDependencyNamesDef,
     el typeDependencyNamesDef,
     el typeNamesInTypeDef]

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
    @@ (ref fullyStripTermDef @@ var "term")

mapBeneathTypeAnnotationsDef :: TElement ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotationsDef = rewritingDefinition "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  lambda "f" $ lambda "t" $
    match _Type (Just $ var "f" @@ var "t") [
      _Type_annotated>>: lambda "at" $ Core.typeAnnotated $ Core.annotatedType
        (ref mapBeneathTypeAnnotationsDef @@ var "f" @@ (Core.annotatedTypeSubject $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at")] @@ var "t"

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
              (Lists.map (asFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs"))],
        _Function_lambda>>: lambda "l" $ list [Core.lambdaBody $ var "l"]],
    _Term_let>>: lambda "lt" $ Lists.cons
      (Core.letEnvironment $ var "lt")
      (Lists.map (asFunction Core.letBindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: lambda "l" $ var "l",
    _Term_literal>>: constant $ list [],
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map (lambda "p" $ list [first $ var "p", second $ var "p"]) (Maps.toList $ var "m"))),
    _Term_optional>>: primitive _optionals_maybe @@  (list []) @@ (lambda "t" $ list [var "t"]),
    _Term_product>>: lambda "tuple" $ var "tuple",
    _Term_record>>: lambda "rt" (Lists.map (asFunction Core.fieldTerm) (Core.recordFields $ var "rt")),
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
    _Term_annotated>>: lambda "at" $ single termAccessorAnnotatedSubject $ Core.annotatedTermSubject $ var "at",
    _Term_application>>: lambda "p" $ list [
      result termAccessorApplicationFunction $ Core.applicationFunction $ var "p",
      result termAccessorApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_function>>: match _Function (Just none) [
        _Function_elimination>>: match _Elimination (Just none) [
            _Elimination_union>>: lambda "cs" $ Lists.concat2
              ((primitive _optionals_maybe @@  none @@ (lambda "t" $ single termAccessorUnionCasesDefault $ var "t"))
                @@ (Core.caseStatementDefault $ var "cs"))
              (Lists.map
                (lambda "f" $ result (termAccessorUnionCasesBranch $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
                (Core.caseStatementCases $ var "cs"))],
        _Function_lambda>>: lambda "l" $ single termAccessorLambdaBody $ Core.lambdaBody $ var "l"],
    _Term_let>>: lambda "lt" $ Lists.cons
      (result termAccessorLetEnvironment $ Core.letEnvironment $ var "lt")
      (Lists.map
        (lambda "b" $ result (termAccessorLetBinding $ Core.letBindingName $ var "b") $ Core.letBindingTerm $ var "b")
        (Core.letBindings $ var "lt")),
    _Term_list>>: lambda "l" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: lambda "m" (Lists.concat
      (Lists.map
        (lambda "p" $ list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (termAccessorMapKey $ int32 0) $ first $ var "p",
          result (termAccessorMapValue $ int32 0) $ second $ var "p"])
        (Maps.toList $ var "m"))),
    _Term_optional>>: primitive _optionals_maybe @@  none @@ (lambda "t" $ single termAccessorOptionalTerm $ var "t"),
    _Term_product>>: lambda "p" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorProductTerm $ int32 0) $ var "e")
      (var "p"),
    _Term_record>>: lambda "rt" (Lists.map
      (lambda "f" $ result (termAccessorRecordField $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
      (Core.recordFields $ var "rt")),
    _Term_set>>: lambda "s" $ Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      (lambda "e" $ result (termAccessorListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_sum>>: lambda "st" $
      single termAccessorSumTerm $
      Core.sumTerm $ var "st",
    _Term_typeAbstraction>>: lambda "ta" $
      single termAccessorTypeAbstractionBody $
      Core.typeAbstractionBody $ var "ta",
    _Term_typeApplication>>: lambda "ta" $
      single termAccessorTypeApplicationTerm $
      Core.typedTermTerm $ var "ta",
    _Term_typed>>: lambda "tt" $
      single termAccessorTypedTerm $
      Core.typedTermTerm $ var "tt",
    _Term_union>>: lambda "ut" $
      single termAccessorInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_variable>>: constant none,
    _Term_wrap>>: lambda "n" $ single termAccessorWrappedTerm $ Core.wrappedTermObject $ var "n"]
  where
    none = list []
    single accessor term = list [result accessor term]
    result accessor term = pair accessor term
    simple term = result termAccessorAnnotatedSubject term

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
    _Type_record>>: lambda "rt" (Lists.map (asFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt")),
    _Type_set>>: lambda "st" $ list [var "st"],
    _Type_sum>>: lambda "st" $ var "st",
    _Type_union>>: lambda "rt" (Lists.map (asFunction Core.fieldTypeType) (Core.rowTypeFields $ var "rt")),
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
          (Logic.or (Logic.not $ var "excludeUnit") (Logic.not $ Core.equalName_ (var "tname") $ Core.name _Unit))
          (Sets.insert (var "tname") (var "names"))
          (var "names"),
      _Type_union>>: lambda "rowType" $ lets [
        "tname">: Core.rowTypeTypeName $ var "rowType"]
        $ Sets.insert (var "tname") (var "names"),
      _Type_wrap>>: lambda "wrappedType" $ lets [
        "tname">: Core.wrappedTypeTypeName $ var "wrappedType"]
        $ Sets.insert (var "tname") (var "names")]]
    $ ref foldOverTypeDef @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty
