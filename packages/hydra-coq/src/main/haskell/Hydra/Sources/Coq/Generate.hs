{-# LANGUAGE FlexibleContexts #-}

-- | Coq code generation driver: per-module pipeline, sentence producers,
-- and cross-module pre-passes. This is the DSL port of the previously
-- hand-written `Hydra.Coq.Generate` in `heads/haskell/`. Anything that
-- needs IO (writing files, creating directories) stays in `heads/`.

module Hydra.Sources.Coq.Generate where

import Hydra.Kernel
import Hydra.Dsl.AsTerm (asTerm)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coq.Syntax                      as CSyntax
import qualified Hydra.Dsl.Packaging                       as Packaging
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Other.Coq                   as CoqSyntax
import qualified Hydra.Sources.Coq.Coder                   as CoqCoderSource
import qualified Hydra.Sources.Coq.Environment             as CoqEnvironmentSource
import qualified Hydra.Sources.Coq.Language                as CoqLanguage
import qualified Hydra.Sources.Coq.Serde                   as CoqSerdeSource
import qualified Hydra.Sources.Coq.Utils                   as CoqUtils
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

import Hydra.Ast
import qualified Hydra.Coq.Syntax as C
import qualified Hydra.Coq.Environment as CE


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.coq.generate"

module_ :: Module
module_ = Module ns definitions
    [CoqUtils.ns, CoqCoderSource.ns, CoqSerdeSource.ns, Formatting.ns, Serialization.ns,
     CoqLanguage.ns, CoqEnvironmentSource.ns, CoqSyntax.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "Coq code generation driver — pre-passes, sentence producers, and per-module pipeline"
  where
    definitions = [
      toDefinition buildAxiomOnlyContent,
      toDefinition buildFullModule,
      toDefinition dependencyImports,
      toDefinition encodeMutualGroupText,
      toDefinition encodeTermGroupSingleton,
      toDefinition generateArgumentsDecls,
      toDefinition generateTypeGroup,
      toDefinition generateTypeSentence,
      toDefinition globalAmbiguousNames,
      toDefinition globalConstructorCounts,
      toDefinition globalFieldMapping,
      toDefinition globalSanitizedAccessors,
      toDefinition implicitArgsLine,
      toDefinition makeAccessorDefs,
      toDefinition makeConstructor,
      toDefinition makeInductiveBody,
      toDefinition makeOneAccessor,
      toDefinition makeProdType,
      toDefinition makeProdVal,
      toDefinition makeProjectionExprs,
      toDefinition makeReturnType,
      toDefinition makeTypeBinder,
      toDefinition mkTypeBinders,
      toDefinition moduleToCoq,
      toDefinition namespaceToPath,
      toDefinition renderRequireImports,
      toDefinition renderSentences,
      toDefinition replaceBundle]

-- | Build the global (qualifiedTypeName, rawFieldName) -> prefixedFieldName
-- map across all modules in the kernel universe.
globalFieldMapping :: TTermDefinition ([Module] -> M.Map (String, String) String)
globalFieldMapping = define "globalFieldMapping" $
  doc "Delegate to CoqUtils.buildFieldMapping across all supplied modules" $
  lambda "modules" $ CoqUtils.buildFieldMapping @@ var "modules"

-- | Build the global map from each union-type name to its constructor count.
-- Used by the encoder's match-exhaustiveness decision.
globalConstructorCounts :: TTermDefinition ([Module] -> M.Map String I.Int32)
globalConstructorCounts = define "globalConstructorCounts" $
  doc "Collect all type definitions from every module and run buildConstructorCounts over them" $
  lambda "modules" $ lets [
    "allTypeDefs">: Lists.concat $ Lists.map
      ("m" ~> Maybes.cat $ Lists.map
        ("def_" ~> cases _Definition (var "def_") (Just (Phantoms.nothing :: TTerm (Maybe (String, Type)))) [
          _Definition_type>>: "td" ~> Phantoms.just $ pair
            (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.typeDefinitionName $ var "td")))
            (Core.typeSchemeType $ Packaging.typeDefinitionType $ var "td")])
        (Packaging.moduleDefinitions $ var "m"))
      (var "modules")] $
    CoqUtils.buildConstructorCounts @@ var "allTypeDefs"

-- | Build the global set of sanitized field accessor names across every
-- module. Accessors listed here came from record fields that were replaced
-- with `unit` to satisfy Coq's strict positivity constraint. The encoder
-- replaces their call sites with `hydra_unreachable`.
globalSanitizedAccessors :: TTermDefinition ([Module] -> S.Set String)
globalSanitizedAccessors = define "globalSanitizedAccessors" $
  doc "Collect sanitized accessor names by SCC-sorting every module's type defs and folding collectSanitizedAccessors" $
  lambda "modules" $ lets [
    "allTypeGroups">: Lists.concat $ Lists.map
      ("m" ~> lets [
        "typeDefs">: Maybes.cat $ Lists.map
          ("def_" ~> cases _Definition (var "def_") (Just (Phantoms.nothing :: TTerm (Maybe (String, Type)))) [
            _Definition_type>>: "td" ~> Phantoms.just $ pair
              (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.typeDefinitionName $ var "td")))
              (Core.typeSchemeType $ Packaging.typeDefinitionType $ var "td")])
          (Packaging.moduleDefinitions $ var "m")] $
        CoqUtils.sortTypeDefsSCC @@ var "typeDefs")
      (var "modules")] $
    CoqUtils.collectSanitizedAccessors @@ var "allTypeGroups"

-- | Build the global set of local names that appear in more than one
-- module. References to such names must remain fully qualified so that
-- Coq's module system resolves the right definition.
globalAmbiguousNames :: TTermDefinition ([Module] -> S.Set String)
globalAmbiguousNames = define "globalAmbiguousNames" $
  doc "Collect local names that occur in more than one module's type or term definitions" $
  lambda "modules" $ lets [
    "allNames">: Lists.concat $ Lists.map
      ("m" ~> lets [
        "nsStr">: Packaging.unNamespace (Packaging.moduleNamespace $ var "m"),
        "fromDef">: lambda "def_" $ cases _Definition (var "def_")
          (Just (Phantoms.nothing :: TTerm (Maybe (String, String)))) [
            _Definition_type>>: "td" ~> Phantoms.just $ pair
              (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.typeDefinitionName $ var "td")))
              (var "nsStr"),
            _Definition_term>>: "td" ~> Phantoms.just $ pair
              (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.termDefinitionName $ var "td")))
              (var "nsStr")]] $
        Maybes.cat $ Lists.map (var "fromDef") (Packaging.moduleDefinitions $ var "m"))
      (var "modules"),
    -- Group by local name, collecting the set of namespaces each name appears in.
    "nameToNs">: Lists.foldl
      ("acc" ~> "np" ~>
        "n" <~ Pairs.first (var "np") $
        "nsVal" <~ Pairs.second (var "np") $
        "existing" <~ Maybes.fromMaybe
          (Sets.empty :: TTerm (S.Set String))
          (Maps.lookup (var "n") (var "acc")) $
        Maps.insert (var "n") (Sets.insert (var "nsVal") (var "existing")) (var "acc"))
      (Maps.empty :: TTerm (M.Map String (S.Set String)))
      (var "allNames")] $
    -- A name is ambiguous iff it appears in >= 2 distinct namespaces.
    Sets.fromList $ Maybes.cat $ Lists.map
      ("entry" ~> Logic.ifElse
        (Equality.gte (Lists.length $ Sets.toList $ Pairs.second $ var "entry") (int32 2))
        (Phantoms.just $ Pairs.first $ var "entry")
        (Phantoms.nothing :: TTerm (Maybe String)))
      (Maps.toList $ var "nameToNs")

-- | Produce the list of Coq `Require Import` sentences for a list of
-- dependency namespace strings. Returns an empty list when the input is
-- empty, otherwise a single `Sentence` containing all deps under one
-- `RequireImport` node with the comment "Module dependencies".
dependencyImports :: TTermDefinition ([String] -> [C.Sentence])
dependencyImports = define "dependencyImports" $
  doc "Emit a Require Import sentence for the given dependency namespaces; empty list yields no sentence" $
  lambda "deps" $
    Logic.ifElse (Lists.null $ var "deps")
      (list ([] :: [TTerm C.Sentence]))
      (list [CSyntax.sentence
        (Phantoms.just $ CSyntax.comment (string "Module dependencies"))
        (CSyntax.sentenceContentRequireImport $ CSyntax.requireImport
          (Phantoms.nothing :: TTerm (Maybe C.Qualid))
          (boolean True)
          (Phantoms.just CSyntax.importQualificationImport)
          (Lists.map (lambda "d" $ CoqCoderSource.coqQualid @@ var "d") (var "deps")))])

-- | Emit the Arguments declarations that follow the type definitions.
-- Only parameterized types produce output; for each one we mark every
-- type parameter implicit on `Build_T`, on each field accessor, and on
-- each union constructor. Returns the Coq source fragment (or empty
-- string if there are no parameterized types), leading with a newline.
generateArgumentsDecls :: TTermDefinition ([(String, Type)] -> String)
generateArgumentsDecls = define "generateArgumentsDecls" $
  doc "Produce Arguments {p} declarations for every parameterized type's constructor and field accessors" $
  "typeDefs" ~>
  -- Build the implicit-all text "{p1} {p2} ..." for a list of parameter names.
  "implicitAll" <~ ("params" ~>
    Strings.intercalate (string " ") $ Lists.map
      ("p" ~> Strings.cat (list [string "{", var "p", string "}"]))
      (var "params")) $
  -- Emit all lines contributed by a single (name, params, bodyTy) triple.
  "linesFor" <~ ("triple" ~>
    "name" <~ (Pairs.first $ var "triple") $
    "params" <~ (Pairs.first $ Pairs.second $ var "triple") $
    "bodyTy" <~ (Pairs.second $ Pairs.second $ var "triple") $
    "impAll" <~ (var "implicitAll" @@ var "params") $
    cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm String])) [
      _Type_union>>: "fields" ~> Lists.map
        ("ft" ~> Strings.cat (list [
          string "Arguments ",
          var "name",
          string "_",
          Formatting.capitalize @@ (unwrap _Name @@ (Core.fieldTypeName $ var "ft")),
          string " ",
          var "impAll",
          string "."]))
        (var "fields"),
      _Type_record>>: "fields" ~>
        Logic.ifElse (Lists.null $ var "fields")
          (list ([] :: [TTerm String]))
          (lets [
            "constrLine">: Strings.cat (list [
              string "Arguments Build_",
              var "name",
              string " ",
              var "impAll",
              string "."]),
            "fieldLines">: Lists.map
              ("ft" ~> Strings.cat (list [
                string "Arguments ",
                Formatting.decapitalize @@ var "name",
                string "_",
                CoqUtils.sanitize @@ (CoqUtils.localName @@ (unwrap _Name @@ (Core.fieldTypeName $ var "ft"))),
                string " ",
                var "impAll",
                string "."]))
              (var "fields")] $
            Lists.cons (var "constrLine") (var "fieldLines"))]) $
  -- For each (name, ty), peel forall binders and only emit lines when there
  -- are actual type parameters to make implicit.
  "triples" <~ (Maybes.cat $ Lists.map
    ("nt" ~>
      "name" <~ (Pairs.first $ var "nt") $
      "ty" <~ (Pairs.second $ var "nt") $
      "ep" <~ (CoqUtils.extractTypeParams @@ var "ty") $
      "params" <~ (Pairs.first $ var "ep") $
      "bodyTy" <~ (Pairs.second $ var "ep") $
      Logic.ifElse (Lists.null $ var "params")
        (Phantoms.nothing :: TTerm (Maybe (String, ([String], Type))))
        (Phantoms.just $ pair (var "name") (pair (var "params") (var "bodyTy"))))
    (var "typeDefs")) $
  "allLines" <~ (Lists.concat $ Lists.map (var "linesFor") (var "triples")) $
  Logic.ifElse (Lists.null $ var "allLines")
    (string "")
    (Strings.cat (list [
      string "\n",
      Strings.intercalate (string "\n") (var "allLines"),
      string "\n"]))

-- | Build the return-type Coq term for a type definition with the given
-- name and parameter-name list. Zero-param: just a qualified reference;
-- otherwise apply the type to each parameter name.
makeReturnType :: TTermDefinition (String -> [String] -> C.Term)
makeReturnType = define "makeReturnType" $
  doc "Return-type Coq term: `TypeName` or `TypeName p1 p2 ...`" $
  lambdas ["typeName", "params"] $
    Logic.ifElse (Lists.null $ var "params")
      (CoqCoderSource.coqTermQualid @@ var "typeName")
      (CoqCoderSource.coqTermApp
        @@ (CoqCoderSource.coqTermQualid @@ var "typeName")
        @@ (Lists.map (lambda "p" $ CoqCoderSource.coqTermQualid @@ var "p") (var "params")))

-- | Build one Coq constructor for a union field: a curried arrow of its
-- field types (just the return type if the field carries `unit`).
makeConstructor :: TTermDefinition (CE.CoqEnvironment -> String -> [String] -> FieldType -> C.Constructor)
makeConstructor = define "makeConstructor" $
  doc "Build a Coq Constructor from a union field (prepended with the type name and capitalized field name)" $
  lambdas ["env", "typeName", "params", "ft"] $
  "fn" <~ (unwrap _Name @@ (Core.fieldTypeName $ var "ft")) $
  "constrName" <~ Strings.cat (list [var "typeName", string "_", Formatting.capitalize @@ var "fn"]) $
  "fieldTy" <~ (Core.fieldTypeType $ var "ft") $
  "argType" <~ (CoqCoderSource.encodeType @@ var "env" @@ var "fieldTy") $
  "returnType" <~ (makeReturnType @@ var "typeName" @@ var "params") $
  CSyntax.constructor
    (CoqCoderSource.coqIdent @@ var "constrName")
    (list ([] :: [TTerm C.Binder]))
    (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqArrow @@ var "argType" @@ var "returnType")

-- | Build one field-accessor Definition sentence for a field at the given
-- index of a record inductive body. The accessor pattern-matches on the
-- `Build_T` constructor and returns the indexed projection variable.
makeOneAccessor :: TTermDefinition (String -> C.Pattern10_Qualid -> [String] -> I.Int32 -> FieldType -> C.Sentence)
makeOneAccessor = define "makeOneAccessor" $
  doc "Emit a Definition for a record field accessor, keyed by the Build_T pattern" $
  lambdas ["typeName", "constrPat", "fieldVars", "idx", "ft"] $
  "fn" <~ (CoqUtils.sanitize @@ (CoqUtils.localName @@ (unwrap _Name @@ (Core.fieldTypeName $ var "ft")))) $
  "prefixedFn" <~ Strings.cat (list [
    Formatting.decapitalize @@ var "typeName",
    string "_",
    var "fn"]) $
  "returnExpr" <~ (CoqCoderSource.coqTermQualid @@
    (Maybes.fromMaybe (string "") (Maps.lookup (var "idx")
      (Maps.fromList $ Lists.zip
        (Math.range (int32 0) (Math.sub (Lists.length $ var "fieldVars") (int32 1)))
        (var "fieldVars"))))) $
  "matchExpr" <~ (inject C._Term C._Term_term100 $
    CSyntax.term100Term10 $
      CSyntax.term10OneTerm $
        CSyntax.oneTermTerm1 $
          CSyntax.term1Term0 $
            CSyntax.term0Match $ CSyntax.match
              (list [CSyntax.caseItem
                (CSyntax.term100Term10 $ CSyntax.term10OneTerm $
                  CSyntax.oneTermExplicit $ CSyntax.qualidAnnotated
                    (CoqCoderSource.coqQualid @@ string "r_")
                    (Phantoms.nothing :: TTerm (Maybe C.UnivAnnot)))
                (Phantoms.nothing :: TTerm (Maybe C.Name))
                (Phantoms.nothing :: TTerm (Maybe C.Pattern))])
              (Phantoms.nothing :: TTerm (Maybe C.Term100))
              (boolean False)
              (list [CSyntax.equation
                (list [list [inject C._Pattern C._Pattern_pattern (CSyntax.pattern10Qualiid $ var "constrPat")]])
                (var "returnExpr")])) $
  CSyntax.sentence
    (Phantoms.nothing :: TTerm (Maybe C.Comment))
    (CSyntax.sentenceContentDefinition $ CSyntax.definition
      (Phantoms.nothing :: TTerm (Maybe C.Locality))
      (CoqCoderSource.coqIdent @@ var "prefixedFn")
      (list [CSyntax.binderType $ CSyntax.typeBinders
        (list [CoqCoderSource.coqName @@ string "r_"])
        (CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ var "typeName")])
      (Phantoms.nothing :: TTerm (Maybe C.Type))
      (var "matchExpr"))

-- | Emit field-accessor Definitions for every field of a record type.
-- Non-record types produce an empty list. Used inside mutual inductive
-- groups where records can't use `Record` syntax, so explicit accessors
-- are generated as separate Definitions.
makeAccessorDefs :: TTermDefinition ((String, Type) -> [C.Sentence])
makeAccessorDefs = define "makeAccessorDefs" $
  doc "Build one Definition per record field, pattern-matching on Build_T" $
  lambda "nt" $
  "name" <~ (Pairs.first $ var "nt") $
  "ty" <~ (Pairs.second $ var "nt") $
  "extracted" <~ (CoqUtils.extractTypeParams @@ var "ty") $
  "bodyTy" <~ (Pairs.second $ var "extracted") $
  cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm C.Sentence])) [
    _Type_record>>: "fields" ~>
      Logic.ifElse (Lists.null $ var "fields")
        (list ([] :: [TTerm C.Sentence]))
        (lets [
          "nFields">: Lists.length $ var "fields",
          "fieldVars">: Lists.map
            (lambda "i" $ Strings.cat (list [string "f", Literals.showInt32 $ var "i"]))
            (Math.range (int32 0) (Math.sub (var "nFields") (int32 1))),
          "constrPat">: CSyntax.pattern10_Qualid
            (CoqCoderSource.coqQualid @@ Strings.cat (list [string "Build_", var "name"]))
            (Lists.map
              (lambda "v" $ CSyntax.pattern1
                (CSyntax.pattern0Qualid $ CoqCoderSource.coqQualid @@ var "v")
                (Phantoms.nothing :: TTerm (Maybe C.ScopeKey)))
              (var "fieldVars")),
          "indexed">: Lists.zip
            (Math.range (int32 0) (Math.sub (var "nFields") (int32 1)))
            (var "fields")] $
          Lists.map (lambda "ift" $ makeOneAccessor
            @@ var "name"
            @@ var "constrPat"
            @@ var "fieldVars"
            @@ (Pairs.first $ var "ift")
            @@ (Pairs.second $ var "ift"))
            (var "indexed"))]

-- | Build an Inductive body for a type in a mutual group: either a
-- sum-type (`TypeUnion`) with one Coq Constructor per field, or a product
-- (`TypeRecord`) with a single `Build_T` constructor whose arrow chain
-- threads the field types to the return type.
makeInductiveBody :: TTermDefinition (CE.CoqEnvironment -> String -> Type -> [C.InductiveBody])
makeInductiveBody = define "makeInductiveBody" $
  doc "Build an Inductive body for a union or record type in a mutual group" $
  lambdas ["env", "name", "ty"] $
  "extracted" <~ (CoqUtils.extractTypeParams @@ var "ty") $
  "params" <~ (Pairs.first $ var "extracted") $
  "bodyTy" <~ (Pairs.second $ var "extracted") $
  "paramBinders" <~ Lists.map
    ("p" ~> CSyntax.binderType $ CSyntax.typeBinders
      (list [CoqCoderSource.coqName @@ var "p"])
      (CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type"))
    (var "params") $
  cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm C.InductiveBody])) [
    _Type_union>>: "fields" ~>
      list [CSyntax.inductiveBody
        (CoqCoderSource.coqIdent @@ var "name")
        (var "paramBinders")
        (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")
        (Lists.map
          (lambda "ft" $ makeConstructor @@ var "env" @@ var "name" @@ var "params" @@ var "ft")
          (var "fields"))],
    _Type_record>>: "fields" ~>
      Logic.ifElse (Lists.null $ var "fields")
        (list [CSyntax.inductiveBody
          (CoqCoderSource.coqIdent @@ var "name")
          (var "paramBinders")
          (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")
          (list [CSyntax.constructor
            (CoqCoderSource.coqIdent @@ Strings.cat (list [string "Build_", var "name"]))
            (list ([] :: [TTerm C.Binder]))
            (Phantoms.just $ CSyntax.type_ $ makeReturnType @@ var "name" @@ var "params")])])
        -- Non-empty record: Build_T with an arrow-chain of field types.
        (lets [
          "constrType">: Lists.foldr
            ("ft" ~> "acc" ~>
              CoqCoderSource.coqArrow
                @@ (CoqCoderSource.encodeType @@ var "env" @@ (Core.fieldTypeType $ var "ft"))
                @@ var "acc")
            (makeReturnType @@ var "name" @@ var "params")
            (var "fields")] $
          list [CSyntax.inductiveBody
            (CoqCoderSource.coqIdent @@ var "name")
            (var "paramBinders")
            (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")
            (list [CSyntax.constructor
              (CoqCoderSource.coqIdent @@ Strings.cat (list [string "Build_", var "name"]))
              (list ([] :: [TTerm C.Binder]))
              (Phantoms.just $ CSyntax.type_ $ var "constrType")])])]

-- | Emit sentences for a single (non-mutual) type definition. Records
-- become Coq `Record`s with prefixed field names; unions become `Inductive`s;
-- other types become `Definition` aliases. Returns a list of sentences
-- so that future extensions can emit multiple related sentences at once.
generateTypeSentence :: TTermDefinition (CE.CoqEnvironment -> String -> Type -> [C.Sentence])
generateTypeSentence = define "generateTypeSentence" $
  doc "Generate the Coq sentence(s) for a non-cyclic type definition" $
  lambdas ["env", "name", "ty"] $
  "extracted" <~ (CoqUtils.extractTypeParams @@ var "ty") $
  "params" <~ (Pairs.first $ var "extracted") $
  "bodyTy" <~ (Pairs.second $ var "extracted") $
  "paramBinders" <~ Lists.map
    ("p" ~> CSyntax.binderType $ CSyntax.typeBinders
      (list [CoqCoderSource.coqName @@ var "p"])
      (CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type"))
    (var "params") $
  "mkDef" <~ ("n" ~> "binders" ~> "body" ~>
    CSyntax.sentence
      (Phantoms.nothing :: TTerm (Maybe C.Comment))
      (CSyntax.sentenceContentDefinition $ CSyntax.definition
        (Phantoms.nothing :: TTerm (Maybe C.Locality))
        (CoqCoderSource.coqIdent @@ var "n")
        (var "binders")
        (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")
        (var "body"))) $
  cases _Type (var "bodyTy") (Just $
    list [var "mkDef" @@ var "name" @@ var "paramBinders"
            @@ (CoqCoderSource.encodeType @@ var "env" @@ var "bodyTy")]) [
    _Type_union>>: "fields" ~> lets [
        "body">: CSyntax.inductiveBody
          (CoqCoderSource.coqIdent @@ var "name")
          (var "paramBinders")
          (Phantoms.just $ CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")
          (Lists.map
            (lambda "ft" $ makeConstructor @@ var "env" @@ var "name" @@ var "params" @@ var "ft")
            (var "fields")),
        "indDef">: CSyntax.inductiveDefinition
          (Phantoms.nothing :: TTerm (Maybe C.Locality))
          (boolean False)
          (list [var "body"])] $
        list [CSyntax.sentence
          (Phantoms.nothing :: TTerm (Maybe C.Comment))
          (CSyntax.sentenceContentInductive $ var "indDef")],
    _Type_record>>: "fields" ~>
      Logic.ifElse (Lists.null $ var "fields")
        (list [var "mkDef" @@ var "name" @@ var "paramBinders"
                @@ (CoqCoderSource.coqTermQualid @@ string "unit")])
        (list [CSyntax.sentence
          (Phantoms.nothing :: TTerm (Maybe C.Comment))
          (CSyntax.sentenceContentRecord $ CSyntax.recordDefinition
            (Phantoms.nothing :: TTerm (Maybe C.Locality))
            (CoqCoderSource.coqIdent @@ var "name")
            (var "paramBinders")
            (Phantoms.just CSyntax.sortType)
            (CSyntax.recordBody
              (Phantoms.just $ CoqCoderSource.coqIdent @@ Strings.cat (list [string "Build_", var "name"]))
              (Lists.map
                (lambda "ft" $ lets [
                  "fn">: CoqUtils.sanitize @@ (CoqUtils.localName @@ (unwrap _Name @@ (Core.fieldTypeName $ var "ft"))),
                  "prefixedFn">: Strings.cat (list [
                    Formatting.decapitalize @@ var "name",
                    string "_",
                    var "fn"]),
                  "ftCoq">: CoqCoderSource.encodeType @@ var "env" @@ (Core.fieldTypeType $ var "ft")] $
                  CSyntax.recordField
                    (CoqCoderSource.coqIdent @@ var "prefixedFn")
                    (CSyntax.type_ $ var "ftCoq"))
                (var "fields"))))])]

-- | Emit sentences for an SCC group of type definitions. Non-cyclic
-- singletons delegate to `generateTypeSentence`. Cyclic groups produce a
-- single `Inductive ... with ...` block plus accessor Definitions for any
-- record members; positivity-violating fields are replaced with `unit`.
generateTypeGroup :: TTermDefinition (CE.CoqEnvironment -> (Bool, [(String, Type)]) -> [C.Sentence])
generateTypeGroup = define "generateTypeGroup" $
  doc "Emit Coq sentences for a type-definition SCC group, handling mutual recursion and positivity" $
  lambdas ["env", "group"] $
  "cyclic" <~ (Pairs.first $ var "group") $
  "defs" <~ (Pairs.second $ var "group") $
  Logic.ifElse
    (Logic.and (Logic.not $ var "cyclic") (Equality.equal (Lists.length $ var "defs") (int32 1)))
    -- Non-cyclic singleton: delegate.
    (lets [
      "d">: Lists.head $ var "defs"] $
      generateTypeSentence @@ var "env" @@ (Pairs.first $ var "d") @@ (Pairs.second $ var "d"))
    -- Mutual group (possibly with positivity sanitization).
    (lets [
      "groupNames">: Sets.fromList $ Lists.map (lambda "d" $ Pairs.first $ var "d") (var "defs"),
      "hasPositivity">: CoqUtils.hasPositivityIssue @@ var "groupNames" @@ var "defs",
      "sanitizedGroup">: Logic.ifElse (var "hasPositivity")
        (Lists.map
          (lambda "d" $ pair
            (Pairs.first $ var "d")
            (CoqUtils.sanitizePositivity @@ var "groupNames" @@ (Pairs.second $ var "d")))
          (var "defs"))
        (var "defs"),
      "bodies">: Lists.concat $ Lists.map
        (lambda "d" $ makeInductiveBody
          @@ var "env"
          @@ (Pairs.first $ var "d")
          @@ (Pairs.second $ var "d"))
        (var "sanitizedGroup"),
      "accessors">: Lists.concat $ Lists.map
        (lambda "d" $ makeAccessorDefs @@ var "d")
        (var "sanitizedGroup"),
      "inductiveSent">: Logic.ifElse (Lists.null $ var "bodies")
        (list ([] :: [TTerm C.Sentence]))
        (list [CSyntax.sentence
          (Phantoms.nothing :: TTerm (Maybe C.Comment))
          (CSyntax.sentenceContentInductive $ CSyntax.inductiveDefinition
            (Phantoms.nothing :: TTerm (Maybe C.Locality))
            (boolean False)
            (var "bodies"))])] $
      Lists.concat2 (var "inductiveSent") (var "accessors"))

-- | Convert a dot-separated Hydra namespace into a relative file path,
-- using `/` as the directory separator and appending `.v`. For example,
-- `"hydra.show.core"` becomes `"hydra/show/core.v"`. The last dotted
-- segment becomes the file name; earlier segments become directories.
namespaceToPath :: TTermDefinition (String -> String)
namespaceToPath = define "namespaceToPath" $
  doc "Convert a Hydra namespace string (e.g. hydra.show.core) into a relative .v file path" $
  lambda "ns" $ lets [
    "parts">: Strings.splitOn (string ".") (var "ns"),
    "dirParts">: Lists.init (var "parts"),
    "fileName">: Strings.cat (list [Lists.last (var "parts"), string ".v"])] $
    Logic.ifElse (Lists.null (var "dirParts"))
      (var "fileName")
      (Strings.cat (list [
        Strings.intercalate (string "/") (var "dirParts"),
        string "/",
        var "fileName"]))

-- | Assemble the full textual body of an axiom-only module. Emits the
-- standard imports header, dependency imports, then one `Axiom` sentence
-- per type definition and one per term definition with a declared scheme.
-- Axiom-only modules bypass term encoding entirely to avoid Coq
-- type-checking memory blowups on heavily polymorphic modules like
-- `hydra.hoisting` and `hydra.inference`.
buildAxiomOnlyContent :: TTermDefinition (CE.CoqEnvironment
                                       -> String
                                       -> String
                                       -> [(String, Type)]
                                       -> [(String, Term, [Name], Maybe Type)]
                                       -> Module
                                       -> String)
buildAxiomOnlyContent = define "buildAxiomOnlyContent" $
  doc "Render an axiom-only Coq module: imports + dependency imports + Axiom declarations" $
  lambdas ["env", "desc", "nsStr", "typeDefs", "termDefs", "mod_"] $
  "typeOfType" <~ (Core.typeVariable (wrap _Name (string "Type"))) $
  "typeAxioms" <~ Lists.map
    ("nt" ~> CoqCoderSource.encodeAxiomDefinitionPair @@ var "env" @@
      pair (Pairs.first $ var "nt") (var "typeOfType"))
    (var "typeDefs") $
  "termAxioms" <~ (Maybes.cat $ Lists.map
    ("td" ~>
      "name" <~ (Pairs.first $ var "td") $
      "mty" <~ (Pairs.second $ Pairs.second $ Pairs.second $ var "td") $
      Maybes.maybe
        (Phantoms.nothing :: TTerm (Maybe C.Sentence))
        ("schemeTy" ~>
          "ep" <~ (CoqUtils.extractTypeParams @@ var "schemeTy") $
          "ty" <~ Pairs.second (var "ep") $
          Phantoms.just $ CoqCoderSource.encodeAxiomDefinitionPair @@ var "env" @@
            pair (var "name") (var "ty"))
        (var "mty"))
    (var "termDefs")) $
  "deps" <~ (CoqUtils.moduleDependencies @@ var "mod_") $
  "depSentences" <~ (dependencyImports @@ var "deps") $
  "allSentences" <~ (Lists.cons (asTerm CoqCoderSource.standardImports)
    (Lists.concat2 (var "depSentences")
      (Lists.concat2 (var "typeAxioms") (var "termAxioms")))) $
  "doc_" <~ (CSyntax.document (var "allSentences")) $
  "body" <~ (Serialization.printExpr @@
    (Serialization.parenthesize @@ (CoqSerdeSource.documentToExpr @@ var "doc_"))) $
  Strings.cat (list [var "desc", var "body", string "\n"])

-- | Build a nested Coq product-type textual expression from a list of type
-- text fragments. `[t]` -> `"t"`; `[t1, t2, ...]` -> `"prod (t1) (prod (...))"`.
-- Empty list defaults to `"unit"`.
makeProdType :: TTermDefinition ([String] -> String)
makeProdType = define "makeProdType" $
  doc "Emit nested `prod (T1) (prod ...)` textual type expression" $
  lambda "ts" $
    Logic.ifElse (Lists.null $ var "ts")
      (string "unit")
      (Logic.ifElse (Equality.equal (Lists.length $ var "ts") (int32 1))
        (Lists.head $ var "ts")
        (Strings.cat (list [
          string "prod (",
          Lists.head $ var "ts",
          string ") (",
          makeProdType @@ (Lists.tail $ var "ts"),
          string ")"])))

-- | Build a nested Coq pair-value textual expression.
-- `[b]` -> `"b"`; multi -> `"(pair (b1) ((pair (b2) (...))))"`.
-- Empty list defaults to `"tt"`.
makeProdVal :: TTermDefinition ([String] -> String)
makeProdVal = define "makeProdVal" $
  doc "Emit a nested `(pair (b1) (...))` textual value expression" $
  lambda "bs" $
    Logic.ifElse (Lists.null $ var "bs")
      (string "tt")
      (Logic.ifElse (Equality.equal (Lists.length $ var "bs") (int32 1))
        (Lists.head $ var "bs")
        (Strings.cat (list [
          string "(pair (",
          Lists.head $ var "bs",
          string ") (",
          makeProdVal @@ (Lists.tail $ var "bs"),
          string "))"])))

-- | Build n projection expressions for a given bundle variable name.
-- n = 0: []; n = 1: [var]; n > 1: for each index 0..n-1, emit
-- `(fst v)`, `(fst (snd v))`, ..., `(snd (... (snd v)))`.
makeProjectionExprs :: TTermDefinition (I.Int32 -> String -> [String])
makeProjectionExprs = define "makeProjectionExprs" $
  doc "Emit the n projection expressions extracting each member of a nested pair bundle" $
  lambdas ["n", "bvar"] $
  "snds" <~ ("k" ~> "v" ~>
    Logic.ifElse (Equality.equal (var "k") (int32 0))
      (var "v")
      (var "snds" @@ (Math.sub (var "k") (int32 1)) @@
        (Strings.cat (list [string "(snd ", var "v", string ")"])))) $
  "mkProj" <~ ("i" ~> "total" ~> "v" ~>
    Logic.ifElse (Equality.equal (var "i") (int32 0))
      (Strings.cat (list [string "(fst ", var "v", string ")"]))
      (Logic.ifElse (Equality.equal (var "i") (Math.sub (var "total") (int32 1)))
        (var "snds" @@ var "i" @@ var "v")
        (Strings.cat (list [string "(fst ", var "snds" @@ var "i" @@ var "v", string ")"])))) $
    Logic.ifElse (Equality.lte (var "n") (int32 0))
      (list ([] :: [TTerm String]))
      (Logic.ifElse (Equality.equal (var "n") (int32 1))
        (list [var "bvar"])
        (Lists.map
          ("i" ~> var "mkProj" @@ var "i" @@ var "n" @@ var "bvar")
          (Math.range (int32 0) (Math.sub (var "n") (int32 1)))))

-- | Replace every occurrence of the literal `"bundle_"` in a string with a
-- given replacement. Implemented by splitting on the sentinel and
-- intercalating with the replacement.
replaceBundle :: TTermDefinition (String -> String -> String)
replaceBundle = define "replaceBundle" $
  doc "Replace literal `bundle_` with the given replacement string" $
  lambdas ["s", "bname"] $
    Strings.intercalate (var "bname") (Strings.splitOn (string "bundle_") (var "s"))

-- | Build a single Coq TypeBinder for a type parameter name.
makeTypeBinder :: TTermDefinition (String -> C.Binder)
makeTypeBinder = define "makeTypeBinder" $
  doc "Build a Coq `(p : Type)` binder for a type parameter" $
  lambda "p" $ CSyntax.binderType $ CSyntax.typeBinders
    (list [CoqCoderSource.coqName @@ var "p"])
    (CSyntax.type_ $ CoqCoderSource.coqTermQualid @@ string "Type")

-- | Compute the full list of type-variable names and corresponding Coq
-- binders for a term definition: union of the explicit scheme variables
-- with any `t0`/`t1`-like variables discovered in the body (only when the
-- scheme already has type parameters — monomorphic functions never hoist).
mkTypeBinders :: TTermDefinition (Term -> [Name] -> ([String], [C.Binder]))
mkTypeBinders = define "mkTypeBinders" $
  doc "Collect type-variable names and the Coq binders needed for a term definition" $
  lambdas ["body", "typeVars"] $
  "schemeVarNames" <~ (Sets.fromList $ Lists.map
    ("n" ~> unwrap _Name @@ var "n") (var "typeVars")) $
  "innerTypeVars" <~ Logic.ifElse (Lists.null $ var "typeVars")
    (Sets.empty :: TTerm (S.Set String))
    (CoqUtils.collectFreeTypeVars @@ var "body") $
  "explicit" <~ (Lists.map ("n" ~> unwrap _Name @@ var "n") (var "typeVars")) $
  "extras" <~ (Lists.filter
    ("nm" ~> Logic.not (Sets.member (var "nm") (var "schemeVarNames")))
    (Sets.toList $ var "innerTypeVars")) $
  "allTypeVarNames" <~ ((Lists.nub :: TTerm [String] -> TTerm [String])
    (Lists.concat2 (var "explicit") (var "extras"))) $
  "binders" <~ Lists.map (lambda "v" $ makeTypeBinder @@ var "v") (var "allTypeVarNames") $
  pair (var "allTypeVarNames") (var "binders")

-- | Emit the "Arguments name {v1} {v2} ...\n" line for a definition with
-- implicit type variable binders. Returns empty string when there are no
-- type variables.
implicitArgsLine :: TTermDefinition (String -> [String] -> String)
implicitArgsLine = define "implicitArgsLine" $
  doc "Emit an Arguments line marking every type parameter of a definition as implicit" $
  lambdas ["name", "typeVarNames"] $
    Logic.ifElse (Lists.null $ var "typeVarNames")
      (string "")
      (Strings.cat (list [
        string "Arguments ",
        var "name",
        string " ",
        Strings.intercalate (string " ")
          (Lists.map ("v" ~> Strings.cat (list [string "{", var "v", string "}"]))
            (var "typeVarNames")),
        string ".\n"]))

-- | Encode a single acyclic term definition as a Coq `Definition` sentence.
-- The body is first passed through `reorderLetBindings` and
-- `eraseUnboundTypeVarDomains` so that only scheme-bound type variables
-- remain as Coq definition parameters.
encodeTermGroupSingleton :: TTermDefinition (CE.CoqEnvironment -> (String, (Term, ([Name], Maybe Type))) -> [C.Sentence])
encodeTermGroupSingleton = define "encodeTermGroupSingleton" $
  doc "Encode a non-cyclic term definition as a Coq Definition sentence" $
  lambdas ["env", "td"] $
  "name" <~ Pairs.first (var "td") $
  "rest1" <~ Pairs.second (var "td") $
  "body" <~ Pairs.first (var "rest1") $
  "rest2" <~ Pairs.second (var "rest1") $
  "typeVars" <~ Pairs.first (var "rest2") $
  "mType" <~ Pairs.second (var "rest2") $
  "schemeVarNames" <~ (Sets.fromList $ Lists.map ("n" ~> unwrap _Name @@ var "n") (var "typeVars")) $
  "body2" <~ (CoqUtils.reorderLetBindings @@
    (CoqUtils.eraseUnboundTypeVarDomains @@ var "schemeVarNames" @@ var "body")) $
  "coqBody" <~ (CoqCoderSource.encodeTerm @@ var "env" @@ var "body2") $
  "binders" <~ (mkTypeBinders @@ var "body2" @@ var "typeVars") $
  "typeBinders" <~ Pairs.second (var "binders") $
  "returnType" <~ Maybes.maybe
    (Phantoms.nothing :: TTerm (Maybe C.Type))
    ("ty" ~>
      "ep" <~ (CoqUtils.extractTypeParams @@ var "ty") $
      "bodyTy" <~ Pairs.second (var "ep") $
      Phantoms.just $ CSyntax.type_ $ CoqCoderSource.encodeType @@ var "env" @@ var "bodyTy")
    (var "mType") $
  list [CSyntax.sentence
    (Phantoms.nothing :: TTerm (Maybe C.Comment))
    (CSyntax.sentenceContentDefinition $ CSyntax.definition
      (Phantoms.nothing :: TTerm (Maybe C.Locality))
      (CoqCoderSource.coqIdent @@ var "name")
      (var "typeBinders")
      (var "returnType")
      (var "coqBody"))]

-- | Render a list of Coq sentences as a textual Document via the Serde
-- layer, parenthesising and pretty-printing the resulting expression.
renderSentences :: TTermDefinition ([C.Sentence] -> String)
renderSentences = define "renderSentences" $
  doc "Pretty-print a Document containing the given Coq sentences" $
  lambda "sentences" $
    Serialization.printExpr @@
      (Serialization.parenthesize @@
        (CoqSerdeSource.documentToExpr @@ (CSyntax.document $ var "sentences")))

-- | Render the Require Import block: the standard imports followed by any
-- user-supplied dependency import sentences.
renderRequireImports :: TTermDefinition ([C.Sentence] -> String)
renderRequireImports = define "renderRequireImports" $
  doc "Pretty-print the standard-imports sentence followed by additional dependency imports" $
  lambda "depSentences" $
    renderSentences @@
      (Lists.cons (asTerm CoqCoderSource.standardImports) (var "depSentences"))

-- | Produce the raw Coq text for a mutually recursive group of term
-- definitions, bundling them via `hydra_fix` over a nested product type.
-- Each (name, body, typeVars, mType) entry is fed into the bundle; the
-- surrounding `letBlock` binds each name to its corresponding projection.
encodeMutualGroupText :: TTermDefinition (CE.CoqEnvironment -> [(String, (Term, ([Name], Maybe Type)))] -> String)
encodeMutualGroupText = define "encodeMutualGroupText" $
  doc "Render a mutually recursive term group as a hydra_fix bundle plus projection Definitions" $
  lambdas ["env", "group"] $
  -- Collect scheme vars across the entire group so each body's unbound-var
  -- erasure uses the union.
  "groupSchemeVars" <~ (Sets.fromList $ Lists.concat $ Lists.map
    ("td" ~>
      "rest1" <~ Pairs.second (var "td") $
      "rest2" <~ Pairs.second (var "rest1") $
      "tv" <~ Pairs.first (var "rest2") $
      Lists.map ("n" ~> unwrap _Name @@ var "n") (var "tv"))
    (var "group")) $
  -- For each entry, produce (name, typeText, bodyText).
  "funInfos" <~ (Lists.map
    ("td" ~>
      "name" <~ Pairs.first (var "td") $
      "rest1" <~ Pairs.second (var "td") $
      "body" <~ Pairs.first (var "rest1") $
      "rest2" <~ Pairs.second (var "rest1") $
      "mType" <~ Pairs.second (var "rest2") $
      "body2" <~ (CoqUtils.reorderLetBindings @@
        (CoqUtils.eraseUnboundTypeVarDomains @@ var "groupSchemeVars" @@ var "body")) $
      "coqBody" <~ (CoqCoderSource.encodeTerm @@ var "env" @@ var "body2") $
      "bodyText" <~ (Serialization.printExpr @@
        (Serialization.parenthesize @@ (CoqSerdeSource.termToExpr @@ var "coqBody"))) $
      "typeText" <~ Maybes.maybe
        (string "_")
        ("ty" ~>
          "ep" <~ (CoqUtils.extractTypeParams @@ var "ty") $
          "bodyTy" <~ Pairs.second (var "ep") $
          Serialization.printExpr @@
            (Serialization.parenthesize @@
              (CoqSerdeSource.typeToExpr @@
                (CSyntax.type_ $ CoqCoderSource.encodeType @@ var "env" @@ var "bodyTy"))))
        (var "mType") $
      pair (var "name") (pair (var "typeText") (var "bodyText")))
    (var "group")) $
  -- Collect type variable binders across the whole group.
  "allTypeVarNames" <~ ((Lists.nub :: TTerm [String] -> TTerm [String]) $ Lists.concat $ Lists.map
    ("td" ~>
      "rest1" <~ Pairs.second (var "td") $
      "b" <~ Pairs.first (var "rest1") $
      "rest2" <~ Pairs.second (var "rest1") $
      "tv" <~ Pairs.first (var "rest2") $
      "binders" <~ (mkTypeBinders @@ var "b" @@ var "tv") $
      Pairs.first (var "binders"))
    (var "group")) $
  "names" <~ Lists.map ("fi" ~> Pairs.first (var "fi")) (var "funInfos") $
  "bundleName" <~ Strings.cat (list [
    Strings.intercalate (string "_") (Lists.take (int32 2) (var "names")),
    string "_bundle"]) $
  "n" <~ Lists.length (var "funInfos") $
  "types" <~ Lists.map
    ("fi" ~> Pairs.first $ Pairs.second $ var "fi")
    (var "funInfos") $
  "productType" <~ (makeProdType @@ var "types") $
  "projExprs" <~ (makeProjectionExprs @@ var "n" @@ string "bundle_") $
  -- Assemble the inner let-block: name1 := proj1 in\n    let name2 := proj2 in\n    ... in\n
  "letParts" <~ Lists.map
    ("np" ~> lets [
      "nm">: Pairs.first $ Pairs.first $ var "np",
      "proj">: Pairs.second $ var "np"] $
      Strings.cat (list [var "nm", string " := ", var "proj"]))
    (Lists.zip (var "funInfos") (var "projExprs")) $
  "letBlock" <~ Logic.ifElse (Lists.null $ var "letParts")
    (string "")
    (Strings.cat (list [
      string "let ",
      Strings.intercalate (string " in\n    let ") (var "letParts"),
      string " in\n    "])) $
  -- Product value: (pair body1 (pair body2 ...))
  "bodies" <~ Lists.map
    ("fi" ~> Pairs.second $ Pairs.second $ var "fi")
    (var "funInfos") $
  "prodVal" <~ (makeProdVal @@ var "bodies") $
  -- Type binder text for the bundle Definition header.
  "typBindText" <~ Logic.ifElse (Lists.null $ var "allTypeVarNames")
    (string "")
    (Strings.cat (list [
      string " ",
      Strings.intercalate (string " ")
        (Lists.map ("v" ~> Strings.cat (list [string "(", var "v", string " : Type)"]))
          (var "allTypeVarNames"))])) $
  "bundleArgsLine" <~ (implicitArgsLine @@ var "bundleName" @@ var "allTypeVarNames") $
  "bundleDef" <~ Strings.cat (list [
    string "Definition ",
    var "bundleName",
    var "typBindText",
    string " :=\n  hydra_fix (fun (bundle_ : ",
    var "productType",
    string ") =>\n    ",
    var "letBlock",
    var "prodVal",
    string ").\n",
    var "bundleArgsLine"]) $
  -- Projection Definitions: one per function in the group.
  "indexed" <~ (Lists.zip
    (Math.range (int32 0) (Math.sub (var "n") (int32 1)))
    (var "funInfos")) $
  "projDefs" <~ Strings.cat (Lists.map
    ("iFi" ~>
      "i" <~ Pairs.first (var "iFi") $
      "fi" <~ Pairs.second (var "iFi") $
      "nm" <~ Pairs.first (var "fi") $
      "t" <~ Pairs.first (Pairs.second (var "fi")) $
      "projText0" <~ Maybes.fromMaybe (string "") (Maps.lookup (var "i")
        (Maps.fromList $ Lists.zip
          (Math.range (int32 0) (Math.sub (var "n") (int32 1)))
          (var "projExprs"))) $
      "projText" <~ (replaceBundle @@ var "projText0" @@ var "bundleName") $
      "argsDef" <~ (implicitArgsLine @@ var "nm" @@ var "allTypeVarNames") $
      Strings.cat (list [
        string "Definition ",
        var "nm",
        var "typBindText",
        string " : ",
        var "t",
        string " :=\n  ",
        var "projText",
        string ".\n",
        var "argsDef"]))
    (var "indexed")) $
  Strings.cat (list [var "bundleDef", string "\n", var "projDefs"])

-- | Assemble the source for a full (non-axiom) Coq module. Walks the type
-- and term definitions through SCC sorting and term-level transforms,
-- produces sentences + per-mutual-group text, and joins the pieces with
-- the standard header and discovered dependency imports. Returns the file
-- content keyed by its relative output path.
buildFullModule :: TTermDefinition (CE.CoqEnvironment
                                  -> M.Map (String, String) String
                                  -> Module
                                  -> String
                                  -> String
                                  -> String
                                  -> [(String, Type)]
                                  -> [(String, (Term, ([Name], Maybe Type)))]
                                  -> M.Map String String)
buildFullModule = define "buildFullModule" $
  doc "Assemble the full (non-axiom) Coq source for a module" $
  lambdas ["env", "fieldMap", "mod_", "nsStr", "path", "desc", "typeDefs", "termDefs"] $
  -- Convert nested-pair termDefs to (name, term) pairs for SCC sorting.
  "termDefsForSort" <~ Lists.map
    ("td" ~> pair (Pairs.first $ var "td") (Pairs.first $ Pairs.second $ var "td"))
    (var "termDefs") $
  "termDefMap" <~ Maps.fromList (Lists.map
    ("td" ~> pair (Pairs.first $ var "td") (Pairs.second $ var "td"))
    (var "termDefs")) $
  "termGroups" <~ (CoqUtils.sortTermDefsSCC @@ var "termDefsForSort") $
  -- Enrich each SCC member with its (term, vars, mType), normalize inner
  -- type lambdas, and rewrite field names.
  "termGroups2" <~ Lists.map
    ("cg" ~>
      "cyc" <~ Pairs.first (var "cg") $
      "grp" <~ Pairs.second (var "cg") $
      "enriched" <~ Maybes.cat (Lists.map
        ("nt" ~>
          "nm" <~ Pairs.first (var "nt") $
          "t" <~ Pairs.second (var "nt") $
          Maybes.map
            ("rec" ~>
              "body2" <~ (CoqUtils.normalizeInnerTypeLambdas @@
                (CoqUtils.rewriteTermFields @@ var "fieldMap" @@ var "t")) $
              -- rec :: (Term, ([Name], Maybe Type)) — we already recomputed the
              -- body via `t`, so we only care about the type-var list and the
              -- optional type scheme from rec.
              "rest" <~ Pairs.second (var "rec") $
              "vs" <~ Pairs.first (var "rest") $
              "mty" <~ Pairs.second (var "rest") $
              pair (var "nm") (pair (var "body2") (pair (var "vs") (var "mty"))))
            (Maps.lookup (var "nm") (var "termDefMap")))
        (var "grp")) $
      pair (var "cyc") (var "enriched"))
    (var "termGroups") $
  -- Type sentences via the already-ported generateTypeGroup.
  "typeGroups" <~ (CoqUtils.sortTypeDefsSCC @@ var "typeDefs") $
  "typeSentences" <~ (Lists.concat $ Lists.map
    ("g" ~> generateTypeGroup @@ var "env" @@ var "g")
    (var "typeGroups")) $
  -- Render term output parts: singletons produce AST sentences (wrapped in
  -- a Document) plus an implicit-Arguments line; cyclic groups produce raw
  -- bundling text via encodeMutualGroupText.
  "termRenderedParts" <~ Lists.concat (Lists.map
    ("cg" ~>
      "cyc" <~ Pairs.first (var "cg") $
      "grp" <~ Pairs.second (var "cg") $
      Logic.ifElse (var "cyc")
        (list [encodeMutualGroupText @@ var "env" @@ var "grp"])
        (Lists.map
          ("td" ~>
            "sentences" <~ (encodeTermGroupSingleton @@ var "env" @@ var "td") $
            "nm" <~ Pairs.first (var "td") $
            "body" <~ Pairs.first (Pairs.second (var "td")) $
            "tv" <~ Pairs.first (Pairs.second (Pairs.second (var "td"))) $
            "schemeVarNames" <~ (Sets.fromList $ Lists.map
              ("n" ~> unwrap _Name @@ var "n") (var "tv")) $
            "body2" <~ (CoqUtils.reorderLetBindings @@
              (CoqUtils.eraseUnboundTypeVarDomains @@ var "schemeVarNames" @@ var "body")) $
            "binders" <~ (mkTypeBinders @@ var "body2" @@ var "tv") $
            "allTypeVarNames" <~ Pairs.first (var "binders") $
            "rendered" <~ (renderSentences @@ var "sentences") $
            "argsLine" <~ Logic.ifElse (Lists.null $ var "allTypeVarNames")
              (string "")
              (Strings.cat (list [
                string "\nArguments ",
                var "nm",
                string " ",
                Strings.intercalate (string " ")
                  (Lists.map ("v" ~> Strings.cat (list [string "{", var "v", string "}"]))
                    (var "allTypeVarNames")),
                string "."])) $
            Strings.cat (list [var "rendered", var "argsLine", string "\n"]))
          (var "grp")))
    (var "termGroups2")) $
  -- Discover cross-module namespace dependencies via the already-ported
  -- walkers.
  "allQualifiedNamesFromTypes" <~ (Sets.unions $ Lists.map
    ("nt" ~> CoqUtils.collectQualifiedNamesInType @@ Pairs.second (var "nt"))
    (var "typeDefs")) $
  "allQualifiedNamesFromTerms" <~ (Sets.unions $ Lists.map
    ("td" ~> CoqUtils.collectQualifiedNamesInTerm @@
      (Pairs.first (Pairs.second (var "td"))))
    (var "termDefs")) $
  "allQualifiedNamesFromTermTypes" <~ (Sets.unions $ Maybes.cat $ Lists.map
    ("td" ~>
      "mty" <~ Pairs.second (Pairs.second (Pairs.second (var "td"))) $
      Maybes.map
        ("ty" ~> lets [
          "ep">: CoqUtils.extractTypeParams @@ var "ty",
          "bodyTy">: Pairs.second (var "ep")] $
          CoqUtils.collectQualifiedNamesInType @@ var "bodyTy")
        (var "mty"))
    (var "termDefs")) $
  "allQualifiedNames" <~ (Sets.union (var "allQualifiedNamesFromTypes")
    (Sets.union (var "allQualifiedNamesFromTerms") (var "allQualifiedNamesFromTermTypes"))) $
  "nsSet" <~ (Sets.fromList $ Lists.map
    ("q" ~> CoqUtils.extractQualifiedNamespace @@ var "q")
    (Sets.toList $ var "allQualifiedNames")) $
  -- Keep only namespaces that aren't the current one and have no strict
  -- suffix present in the set (avoid importing both `hydra.foo` and
  -- `hydra.foo.bar` redundantly).
  "strStartsWith" <~ ("pref" ~> "s" ~>
    Logic.and
      (Equality.gte (Strings.length $ var "s") (Strings.length $ var "pref"))
      (Equality.equal
        (Strings.fromList $ Lists.take (Strings.length $ var "pref") (Strings.toList $ var "s"))
        (var "pref"))) $
  "hasStrictSuffix" <~ ("nsC" ~> "otherList" ~>
    Maybes.isJust $ Lists.find
      ("other" ~> Logic.and
        (Logic.not (Equality.equal (var "other") (var "nsC")))
        (var "strStartsWith" @@
          (Strings.cat (list [var "nsC", string "."])) @@
          (var "other")))
      (var "otherList")) $
  "referencedNs" <~ ((Lists.nub :: TTerm [String] -> TTerm [String]) $ Lists.filter
    ("nsC" ~> Logic.and
      (Logic.not (Equality.equal (var "nsC") (var "nsStr")))
      (Logic.not (var "hasStrictSuffix" @@ var "nsC" @@ (Sets.toList $ var "nsSet"))))
    (Sets.toList $ var "nsSet")) $
  "depSentences" <~ (dependencyImports @@ var "referencedNs") $
  -- Assemble the final output: header + imports + type sentences + term
  -- parts + Arguments declarations for parameterized types.
  "importText" <~ (renderRequireImports @@ var "depSentences") $
  "typeSentencesText" <~ (renderSentences @@ var "typeSentences") $
  "allTermText" <~ (Strings.cat $ var "termRenderedParts") $
  "typeArgsDecls" <~ (generateArgumentsDecls @@ var "typeDefs") $
  "content" <~ Strings.cat (list [
    var "desc",
    var "importText",
    string "\n",
    var "typeSentencesText",
    string "\n",
    var "allTermText",
    var "typeArgsDecls",
    string "\n"]) $
  Maps.fromList (list [pair (var "path") (var "content")])

-- | Standard coder entry point: given the cross-module pre-pass results
-- and a single adapted module, produce the map from output file path to
-- generated Coq source text. `moduleToCoq` either delegates to
-- `buildFullModule` or (for modules that exceed Coq's practical
-- type-checking limits) emits an axiom-only stub via
-- `buildAxiomOnlyContent`.
moduleToCoq :: TTermDefinition (M.Map (String, String) String
                              -> M.Map String I.Int32
                              -> S.Set String
                              -> S.Set String
                              -> Module
                              -> [Definition]
                              -> M.Map String String)
moduleToCoq = define "moduleToCoq" $
  doc "Top-level driver: dispatch a module to either full-emission or axiom-only emission, producing (path, content) pairs" $
  lambdas ["fieldMap", "constrCounts", "ambiguousNames", "globalSanitizedAcc", "mod_", "defs"] $
  "nsStr" <~ (Packaging.unNamespace (Packaging.moduleNamespace $ var "mod_")) $
  "path" <~ (namespaceToPath @@ var "nsStr") $
  "desc" <~ Maybes.maybe
    (string "")
    ("d" ~> Strings.cat (list [string "(* ", var "d", string " *)\n\n"]))
    (Packaging.moduleDescription $ var "mod_") $
  -- Modules known to blow up Coq's type-checker; emit axiom stubs instead.
  "axiomOnlyModules" <~ (list [string "hydra.hoisting", string "hydra.inference"]) $
  "isAxiomOnly" <~ ((Lists.elem :: TTerm String -> TTerm [String] -> TTerm Bool)
    (var "nsStr") (var "axiomOnlyModules")) $
  -- Extract type and term definitions from the adapted definition list.
  "typeDefs" <~ Maybes.cat (Lists.map
    ("def_" ~> cases _Definition (var "def_") (Just (Phantoms.nothing :: TTerm (Maybe (String, Type)))) [
      _Definition_type>>: "td" ~> Phantoms.just $ pair
        (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.typeDefinitionName $ var "td")))
        (Core.typeSchemeType $ Packaging.typeDefinitionType $ var "td")])
    (var "defs")) $
  "termDefs" <~ Maybes.cat (Lists.map
    ("def_" ~> cases _Definition (var "def_")
      (Just (Phantoms.nothing :: TTerm (Maybe (String, (Term, ([Name], Maybe Type)))))) [
      _Definition_term>>: "td" ~>
        "mts" <~ (Packaging.termDefinitionType $ var "td") $
        "vs" <~ Maybes.maybe
          (list ([] :: [TTerm Name]))
          ("ts" ~> Core.typeSchemeVariables $ var "ts")
          (var "mts") $
        "mty" <~ Maybes.map ("ts" ~> Core.typeSchemeType $ var "ts") (var "mts") $
        Phantoms.just $ pair
          (CoqUtils.localName @@ (unwrap _Name @@ (Packaging.termDefinitionName $ var "td")))
          (pair
            (Packaging.termDefinitionTerm $ var "td")
            (pair (var "vs") (var "mty")))])
    (var "defs")) $
  -- Local definition names (both type and term), used to extend the
  -- ambiguous-names set so that cross-module references to those names
  -- stay fully qualified.
  "localDefNames" <~ (Sets.fromList $ Lists.concat2
    (Lists.map (lambda "nt" $ Pairs.first $ var "nt") (var "typeDefs"))
    (Lists.map (lambda "td" $ Pairs.first $ var "td") (var "termDefs"))) $
  "moduleAmbig" <~ (Sets.union (var "ambiguousNames") (var "localDefNames")) $
  -- Construct the CoqEnvironment record that threads through every encoder.
  "env" <~ (record CE._CoqEnvironment [
    CE._CoqEnvironment_currentNamespace>>: var "nsStr",
    CE._CoqEnvironment_constructorCounts>>: var "constrCounts",
    CE._CoqEnvironment_ambiguousNames>>: var "moduleAmbig",
    CE._CoqEnvironment_sanitizedAccessors>>: var "globalSanitizedAcc"]) $
  Logic.ifElse (var "isAxiomOnly")
    (Maps.fromList (list [pair
      (var "path")
      (buildAxiomOnlyContent @@ var "env" @@ var "desc" @@ var "nsStr"
         @@ var "typeDefs" @@ var "termDefs" @@ var "mod_")]))
    (buildFullModule @@ var "env" @@ var "fieldMap" @@ var "mod_"
       @@ var "nsStr" @@ var "path" @@ var "desc" @@ var "typeDefs" @@ var "termDefs")





