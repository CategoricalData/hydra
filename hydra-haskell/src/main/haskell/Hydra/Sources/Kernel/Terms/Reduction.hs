{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Reduction where

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

import qualified Hydra.Sources.Kernel.Terms.Arity as Arity
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


module_ :: Module
module_ = Module (Namespace "hydra.reduction") elements
    [Arity.module_, ExtractCore.module_, Lexical.module_, Rewriting.module_,
      Schemas.module_]
    kernelTypesModules $
    Just ("Functions for reducing terms and types, i.e. performing computations.")
  where
   elements = [
     el alphaConvertDef,
     el betaReduceTypeDef,
     el contractTermDef,
     el countPrimitiveInvocationsDef,
     el etaReduceTermDef,
     el expandLambdasDef,
     el expansionArityDef,
     el reduceTermDef,
     el termIsClosedDef,
     el termIsValueDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

alphaConvertDef :: TElement (Name -> Name -> Term -> Term)
alphaConvertDef = define "alphaConvert" $
  doc "Alpha convert a variable in a term" $
  "vold" ~> "vnew" ~> "term" ~> ref Rewriting.replaceFreeTermVariableDef @@ var "vold" @@ (Core.termVariable $ var "vnew") @@ var "term"

-- Note: this is eager beta reduction, in that we always descend into subtypes,
--       and always reduce the right-hand side of an application prior to substitution
betaReduceTypeDef :: TElement (Type -> Flow Graph Type)
betaReduceTypeDef = define "betaReduceType" $
  lambda "typ" $ lets [
    "mapExpr">: lambdas ["recurse", "t"] $
      Flows.bind (var "recurse" @@ var "t") $
        lambda "r" $
          match _Type (Just $ Flows.pure $ var "r") [
            _Type_application>>: lambda "a" $ var "reduceApp" @@ var "a"] @@ var "r",
    "reduceApp">: lambda "app" $ lets [
      "lhs">: Core.applicationTypeFunction $ var "app",
      "rhs">: Core.applicationTypeArgument $ var "app"]
      $ match _Type Nothing [
        _Type_annotated>>: lambda "at" $
          Flows.bind (var "reduceApp" @@ (Core.applicationType
            (Core.annotatedTypeSubject $ var "at")
            (var "rhs"))) $
            lambda "a" $ Flows.pure $ Core.typeAnnotated $ Core.annotatedType (var "a") (Core.annotatedTypeAnnotation $ var "at"),
        _Type_forall>>: lambda "ft" $
          ref betaReduceTypeDef @@ (ref Rewriting.replaceFreeTypeVariableDef
            @@ (Core.forallTypeParameter $ var "ft")
            @@ var "rhs"
            @@ (Core.forallTypeBody $ var "ft")),
        _Type_variable>>: lambda "name" $
          Flows.bind (ref Schemas.requireTypeDef @@ var "name") $
            lambda "t'" $ ref betaReduceTypeDef @@ (Core.typeApplication $ Core.applicationType (var "t'") (var "rhs"))] @@ var "lhs"]
    $ ref Rewriting.rewriteTypeMDef @@ var "mapExpr" @@ var "typ"

contractTermDef :: TElement (Term -> Term)
contractTermDef = define "contractTerm" $
  doc ("Apply the special rules:\n"
    <> "    ((\\x.e1) e2) == e1, where x does not appear free in e1\n"
    <> "  and\n"
    <> "     ((\\x.e1) e2) = e1[x/e2]\n"
    <> "These are both limited forms of beta reduction which help to \"clean up\" a term without fully evaluating it.") $
  "term" ~> lets [
    "rewrite">: "recurse" ~> "t" ~> lets [
      "rec">: var "recurse" @@ var "t"] $
      cases _Term (var "rec")
        (Just $ var "rec") [
        _Term_application>>: "app" ~> lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app"] $
          cases _Term (ref Rewriting.deannotateTermDef @@ var "lhs")
            (Just $ var "rec") [
            _Term_function>>: "f" ~> cases _Function (var "f")
              (Just $ var "rec") [
              _Function_lambda>>: "l" ~> lets [
                "v">: Core.lambdaParameter $ var "l",
                "body">: Core.lambdaBody $ var "l"] $
                Logic.ifElse (ref Rewriting.isFreeVariableInTermDef @@ var "v" @@ var "body")
                  (var "body")
                  (ref Rewriting.replaceFreeTermVariableDef @@ var "v" @@ var "rhs" @@ var "body")]]]] $
    ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "term"

-- For demo purposes. This should be generalized to enable additional side effects of interest.
countPrimitiveInvocationsDef :: TElement Bool
countPrimitiveInvocationsDef = define "countPrimitiveInvocations" true

-- Note: unused / untested
etaReduceTermDef :: TElement (Term -> Term)
etaReduceTermDef = define "etaReduceTerm" $
  lambda "term" $ lets [
    "noChange">: var "term",
    "reduceLambda">: lambda "l" $ lets [
      "v">: Core.lambdaParameter $ var "l",
      "d">: Core.lambdaDomain $ var "l",
      "body">: Core.lambdaBody $ var "l"]
      $ match _Term (Just $ var "noChange") [
        _Term_annotated>>: lambda "at" $
          var "reduceLambda" @@ (Core.lambda (var "v") (var "d") (Core.annotatedTermSubject $ var "at")),
        _Term_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app"]
          $ match _Term (Just $ var "noChange") [
            _Term_annotated>>: lambda "at" $
              var "reduceLambda" @@ (Core.lambda (var "v") (var "d") $
                Core.termApplication $ Core.application (var "lhs") (Core.annotatedTermSubject $ var "at")),
            _Term_variable>>: lambda "v1" $
              Logic.ifElse
                (Logic.and
                  (Equality.equal (Core.unName $ var "v") (Core.unName $ var "v1"))
                  (Logic.not $ ref Rewriting.isFreeVariableInTermDef @@ var "v" @@ var "lhs"))
                (ref etaReduceTermDef @@ var "lhs")
                (var "noChange")]
          @@ (ref etaReduceTermDef @@ var "rhs")]
      @@ (ref etaReduceTermDef @@ var "body")]
    $ match _Term (Just $ var "noChange") [
      _Term_annotated>>: lambda "at" $
        Core.termAnnotated $ Core.annotatedTerm
          (ref etaReduceTermDef @@ (Core.annotatedTermSubject $ var "at"))
          (Core.annotatedTermAnnotation $ var "at"),
      _Term_function>>: lambda "f" $
        match _Function (Just $ var "noChange") [
          _Function_lambda>>: lambda "l" $ var "reduceLambda" @@ var "l"]
        @@ var "f"]
    @@ var "term"

expandLambdasDef :: TElement (Graph -> Term -> Term)
expandLambdasDef = define "expandLambdas" $
  doc ("Recursively transform arbitrary terms like 'add 42' into terms like '\\x.add 42 x', in which the implicit"
    <> " parameters of primitive functions and eliminations are made into explicit lambda parameters."
    <> " Variable references are not expanded."
    <> " This is useful for targets like Python with weaker support for currying than Hydra or Haskell."
    <> " Note: this is a \"trusty\" function which assumes the graph is well-formed, i.e. no dangling references.") $
  "graph" ~> "term" ~> lets [
    "expand">: "args" ~> "arity" ~> "t" ~> lets [
      "apps">: Lists.foldl
        ("lhs" ~> "arg" ~> Core.termApplication $ Core.application (var "lhs") (var "arg"))
        (var "t")
        (var "args"),
      "is">: Logic.ifElse (Equality.lte (var "arity") (Lists.length $ var "args"))
        (list [])
        (Math.range (int32 1) (Math.sub (var "arity") (Lists.length $ var "args"))),
      "pad">: "indices" ~> "t" ~>
        Logic.ifElse (Lists.null $ var "indices")
          (var "t")
          (Core.termFunction $ Core.functionLambda $
            Core.lambda (Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ Lists.head $ var "indices")) nothing $
              var "pad" @@ Lists.tail (var "indices") @@
                (Core.termApplication $ Core.application (var "t") $ Core.termVariable $
                  Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ Lists.head $ var "indices")))] $
      var "pad" @@ var "is" @@ var "apps",
    "rewrite">: "args" ~> "recurse" ~> "t" ~> lets [
      "afterRecursion">: "term" ~>
        var "expand" @@ var "args" @@ (ref expansionArityDef @@ var "graph" @@ var "term") @@ var "term"] $
      cases _Term (var "t")
        (Just $ var "afterRecursion" @@ (var "recurse" @@ var "t")) [
        _Term_application>>: "app" ~> lets [
          "lhs">: Core.applicationFunction $ var "app",
          "rhs">: Core.applicationArgument $ var "app",
          "erhs">: var "rewrite" @@ (list []) @@ var "recurse" @@ var "rhs"] $
          var "rewrite" @@ (Lists.cons (var "erhs") (var "args")) @@ var "recurse" @@ var "lhs"]]
    $ ref contractTermDef @@ (ref Rewriting.rewriteTermDef @@ (var "rewrite" @@ (list [])) @@ var "term")

expansionArityDef :: TElement (Graph -> Term -> Int)
expansionArityDef = define "expansionArity" $
  doc "Calculate the arity for lambda expansion" $
  "graph" ~> "term" ~>
    cases _Term (ref Rewriting.deannotateTermDef @@ var "term")
      (Just $ int32 0) [
      _Term_application>>: lambda "app" $
        Math.sub
          (ref expansionArityDef @@ var "graph" @@ Core.applicationFunction (var "app"))
          (int32 1),
      _Term_function>>: "f" ~> cases _Function (var "f")
        Nothing [
        _Function_elimination>>: constant $ int32 1,
        _Function_lambda>>: constant $ int32 0,
        _Function_primitive>>: "name" ~>
          ref Arity.primitiveArityDef @@ (Optionals.fromJust (ref Lexical.lookupPrimitiveDef @@ var "graph" @@ var "name"))],
      _Term_typeAbstraction>>: "ta" ~> ref expansionArityDef @@ var "graph" @@ Core.typeAbstractionBody (var "ta"),
      _Term_typeApplication>>: "tt" ~> ref expansionArityDef @@ var "graph" @@ Core.typedTermTerm (var "tt"),
      _Term_variable>>: "name" ~>
        Optionals.maybe (int32 0)
          ("ts" ~> ref Arity.typeArityDef @@ (Core.typeSchemeType $ var "ts"))
          (Optionals.bind
            (ref Lexical.lookupElementDef @@ var "graph" @@ var "name")
            ("el" ~> Graph.elementType $ var "el"))]

reduceTermDef :: TElement (Bool -> Term -> Flow Graph Term)
reduceTermDef = define "reduceTerm" $
  doc "A term evaluation function which is alternatively lazy or eager" $
  lambdas ["eager", "term"] $ lets [
    "reduce">: lambda "eager" $ ref reduceTermDef @@ var "eager",

    "doRecurse">: lambdas ["eager", "term"] $
      Logic.and (var "eager") $ cases _Term (var "term") (Just true) [
        _Term_function>>: match _Function (Just true) [
          _Function_lambda>>: constant false]],

    "reduceArg">: lambdas ["eager", "arg"] $
      Logic.ifElse (var "eager")
        (Flows.pure $ var "arg")
        (var "reduce" @@ false @@ var "arg"),

    "applyToArguments">: lambdas ["fun", "args"] $
      Logic.ifElse (Lists.null $ var "args")
        (var "fun")
        (var "applyToArguments" @@
          (Core.termApplication $ Core.application (var "fun") (Lists.head $ var "args")) @@
          (Lists.tail $ var "args")),

    "replaceFreeTypeVariable">: lambdas ["toReplace", "replacement", "term"] $ lets [
      "mapping">: lambdas ["recurse", "inner"] $
        match _Term (Just $ var "recurse" @@ var "inner") [
          _Term_function>>: match _Function (Just $ var "recurse" @@ var "inner") [
            _Function_lambda>>: lambda "l" $ Logic.ifElse
              (Equality.equal (Core.lambdaParameter $ var "l") (var "toReplace"))
              (var "inner")
              (var "recurse" @@ var "inner")],
          _Term_variable>>: lambda "name" $ Logic.ifElse
            (Equality.equal (var "name") (var "toReplace"))
            (var "replacement")
            (var "inner")] @@ var "inner"]
      $ ref Rewriting.rewriteTermDef @@ var "mapping" @@ var "term",

    "applyElimination">: lambdas ["elm", "reducedArg"] $
      match _Elimination Nothing [
        _Elimination_record>>: lambda "proj" $
          Flows.bind (ref ExtractCore.recordDef @@ (Core.projectionTypeName $ var "proj") @@ (ref Rewriting.deannotateTermDef @@ var "reducedArg")) $
            lambda "fields" $ lets [
              "matchingFields">: Lists.filter
                (lambda "f" $ Equality.equal (Core.fieldName $ var "f") (Core.projectionField $ var "proj"))
                (var "fields")]
              $ Logic.ifElse
                (Lists.null $ var "matchingFields")
                (Flows.fail $ Strings.cat $ list [
                  string "no such field: ",
                  unwrap _Name @@ (Core.projectionField $ var "proj"),
                  string " in ",
                  unwrap _Name @@ (Core.projectionTypeName $ var "proj"),
                  string " record"])
                (Flows.pure $ Core.fieldTerm $ Lists.head $ var "matchingFields"),
        _Elimination_union>>: lambda "cs" $
          Flows.bind (ref ExtractCore.injectionDef @@ (Core.caseStatementTypeName $ var "cs") @@ var "reducedArg") $
            lambda "field" $ lets [
              "matchingFields">: Lists.filter
                (lambda "f" $ Equality.equal (Core.fieldName $ var "f") (Core.fieldName $ var "field"))
                (Core.caseStatementCases $ var "cs")]
              $ Logic.ifElse (Lists.null $ var "matchingFields")
                (Optionals.maybe
                  (Flows.fail $ Strings.cat $ list [
                    string "no such field ",
                    unwrap _Name @@ (Core.fieldName $ var "field"),
                    string " in ",
                    unwrap _Name @@ (Core.caseStatementTypeName $ var "cs"),
                    string " case statement"])
                  (unaryFunction Flows.pure)
                  (Core.caseStatementDefault $ var "cs"))
                (Flows.pure $ Core.termApplication $ Core.application
                  (Core.fieldTerm $ Lists.head $ var "matchingFields")
                  (Core.fieldTerm $ var "field")),
        _Elimination_wrap>>: lambda "name" $ ref ExtractCore.wrapDef @@ var "name" @@ var "reducedArg"] @@ var "elm",

    "applyIfNullary">: lambdas ["eager", "original", "args"] $ lets [
      "stripped">: ref Rewriting.deannotateTermDef @@ var "original"]
      $ cases _Term (var "stripped") (Just $ Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args") [
        _Term_application>>: lambda "app" $ var "applyIfNullary" @@ var "eager" @@
          (Core.applicationFunction $ var "app") @@
          (Lists.cons (Core.applicationArgument $ var "app") (var "args")),
        _Term_function>>: match _Function Nothing [
            _Function_elimination>>: lambda "elm" $
              Logic.ifElse (Lists.null $ var "args")
                (Flows.pure $ var "original")
                (lets [
                  "arg">: Lists.head $ var "args",
                  "remainingArgs">: Lists.tail $ var "args"]
                  $ Flows.bind (var "reduceArg" @@ var "eager" @@ (ref Rewriting.deannotateTermDef @@ var "arg")) $
                    lambda "reducedArg" $
                      Flows.bind (Flows.bind (var "applyElimination" @@ var "elm" @@ var "reducedArg") (var "reduce" @@ var "eager")) $
                        lambda "reducedResult" $ var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs"),
            _Function_lambda>>: lambda "l" $
              Logic.ifElse (Lists.null $ var "args")
                (Flows.pure $ var "original")
                (lets [
                  "param">: Core.lambdaParameter $ var "l",
                  "body">: Core.lambdaBody $ var "l",
                  "arg">: Lists.head $ var "args",
                  "remainingArgs">: Lists.tail $ var "args"]
                  $ Flows.bind (var "reduce" @@ var "eager" @@ (ref Rewriting.deannotateTermDef @@ var "arg")) $
                    lambda "reducedArg" $
                      Flows.bind (var "reduce" @@ var "eager" @@ (var "replaceFreeTypeVariable" @@ var "param" @@ var "reducedArg" @@ var "body")) $
                        lambda "reducedResult" $ var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs"),
            _Function_primitive>>: lambda "name" $
              Flows.bind (ref Lexical.requirePrimitiveDef @@ var "name") $ lambda "prim" $
                lets [
                  "arity">: ref Arity.primitiveArityDef @@ var "prim"]
                  $ Logic.ifElse (Equality.gt (var "arity") (Lists.length $ var "args"))
                    (Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args")
                    (lets [
                      "argList">: Lists.take (var "arity") (var "args"),
                      "remainingArgs">: Lists.drop (var "arity") (var "args")]
                      $ Flows.bind (Flows.mapList (var "reduceArg" @@ var "eager") (var "argList")) $ lambda "reducedArgs" $
                          Flows.bind
                            (Flows.bind
                              (Graph.primitiveImplementation (var "prim") @@ var "reducedArgs")
                              (var "reduce" @@ var "eager")) $ lambda "reducedResult" $
                                var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs")],
        _Term_variable>>: lambda "v" $ Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args"],
    "mapping">: lambdas ["recurse", "mid"] $
      Flows.bind
        (Logic.ifElse (var "doRecurse" @@ var "eager" @@ var "mid")
          (var "recurse" @@ var "mid")
          (Flows.pure $ var "mid")) $
        lambda "inner" $ var "applyIfNullary" @@ var "eager" @@ var "inner" @@ (list [])]
    $ ref Rewriting.rewriteTermMDef @@ var "mapping" @@ var "term"

termIsClosedDef :: TElement (Term -> Bool)
termIsClosedDef = define "termIsClosed" $
  doc "Whether a term is closed, i.e. represents a complete program" $
  lambda "term" $ Sets.null $ ref Rewriting.freeVariablesInTermDef @@ var "term"

termIsValueDef :: TElement (Graph -> Term -> Bool)
termIsValueDef = define "termIsValue" $
  doc "Whether a term has been fully reduced to a value" $
  lambda "g" $ lambda "term" $ lets [
    "forList">: lambda "els" $ Lists.foldl (lambda "b" $ lambda "t" $ Logic.and (var "b") (ref termIsValueDef @@ var "g" @@ var "t")) true (var "els"),
    "checkField">: lambda "f" $ ref termIsValueDef @@ var "g" @@ Core.fieldTerm (var "f"),
    "checkFields">: lambda "fields" $ Lists.foldl (lambda "b" $ lambda "f" $ Logic.and (var "b") (var "checkField" @@ var "f")) true (var "fields"),
    "functionIsValue">: lambda "f" $
      match _Function Nothing [
        _Function_elimination>>: lambda "e" $
          match _Elimination Nothing [
            _Elimination_wrap>>: constant true,
            _Elimination_record>>: constant true,
            _Elimination_union>>: lambda "cs" $
              Logic.and (var "checkFields" @@ Core.caseStatementCases (var "cs"))
                (Optionals.maybe true (ref termIsValueDef @@ var "g") (Core.caseStatementDefault $ var "cs"))]
          @@ var "e",
        _Function_lambda>>: lambda "l" $ ref termIsValueDef @@ var "g" @@ Core.lambdaBody (var "l"),
        _Function_primitive>>: constant true]
      @@ var "f"]
    $ match _Term (Just false) [
      _Term_application>>: constant false,
      _Term_literal>>: constant true,
      _Term_function>>: lambda "f" $ var "functionIsValue" @@ var "f",
      _Term_list>>: lambda "els" $ var "forList" @@ var "els",
      _Term_map>>: lambda "m" $
        Lists.foldl (lambda "b" $ lambda "kv" $
          Logic.and (var "b") $ Logic.and
            (ref termIsValueDef @@ var "g" @@ first (var "kv"))
            (ref termIsValueDef @@ var "g" @@ second (var "kv")))
          true $ Maps.toList (var "m"),
      _Term_optional>>: lambda "m" $
        Optionals.maybe true (ref termIsValueDef @@ var "g") (var "m"),
      _Term_record>>: lambda "r" $ var "checkFields" @@ Core.recordFields (var "r"),
      _Term_set>>: lambda "s" $ var "forList" @@ Sets.toList (var "s"),
      _Term_union>>: lambda "i" $ var "checkField" @@ Core.injectionField (var "i"),
      _Term_unit>>: constant true,
      _Term_variable>>: constant false]
    @@ (ref Rewriting.deannotateTermDef @@ var "term")
