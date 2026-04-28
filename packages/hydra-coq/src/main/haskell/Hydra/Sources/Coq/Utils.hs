{-# LANGUAGE FlexibleContexts #-}

-- | Pure helper functions for the Coq code generator. These were previously
-- hand-written Haskell in `heads/haskell/.../Hydra/Coq/Generate.hs`; this
-- module promotes them to DSL form so that the host-side Generate.hs shrinks
-- to an IO / per-module driver. See issue #337.

module Hydra.Sources.Coq.Utils where

import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Packaging                       as Packaging
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Coq.Language                as CoqLanguage
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.coq.utils"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Formatting.ns, Rewriting.ns, Sorting.ns, Variables.ns, CoqLanguage.ns],
            moduleTypeDependencies = KernelTypes.kernelTypesNamespaces,
            moduleDescription = Just "Pure helpers for the Coq code generator"}
  where
    definitions = [
      toDefinition buildConstructorCounts,
      toDefinition buildFieldMapping,
      toDefinition collectFreeTypeVars,
      toDefinition collectFreeTypeVarsInType,
      toDefinition collectFreeTypeVarsInTypeScheme,
      toDefinition collectLetBindings,
      toDefinition collectQualifiedNamesInTerm,
      toDefinition collectQualifiedNamesInType,
      toDefinition collectQualifiedNamesInTypeScheme,
      toDefinition collectSanitizedAccessors,
      toDefinition encodeMutualLetGroup,
      toDefinition eraseUnboundTypeVarDomains,
      toDefinition extractQualifiedNamespace,
      toDefinition extractTypeParams,
      toDefinition fieldCausesPositivityIssue,
      toDefinition hasPositivityIssue,
      toDefinition hasUnboundTypeVar,
      toDefinition isTypeLambdaTerm,
      toDefinition isTypeVarLike,
      toDefinition localName,
      toDefinition localNameRaw,
      toDefinition moduleDependencies,
      toDefinition normalizeInnerTypeLambdas,
      toDefinition processLetSCCs,
      toDefinition qualifiedFromName,
      toDefinition rebuildLets,
      toDefinition rebuildMutualLets,
      toDefinition reorderLetBindings,
      toDefinition rewriteTermFields,
      toDefinition sanitize,
      toDefinition sanitizePositivity,
      toDefinition sortTermDefsSCC,
      toDefinition sortTypeDefsSCC,
      toDefinition stripHydraFix,
      toDefinition targetsPolyName,
      toDefinition termRefs,
      toDefinition typeContainsGroupRef,
      toDefinition typeRefs,
      toDefinition typeToTerm]

-- | Extract the qualified-namespace part of a Hydra Name string.
-- `"hydra.core.Term_Literal"` -> `"hydra.core"`.
-- `"hydra.lib.strings.cat"` -> `"hydra.lib.strings"`.
-- Names without at least one dot are returned unchanged.
extractQualifiedNamespace :: TTermDefinition (String -> String)
extractQualifiedNamespace = define "extractQualifiedNamespace" $
  doc "Extract the namespace (everything except the last dot-separated component) from a qualified Hydra name" $
  lambda "s" $ lets [
    "parts">: Strings.splitOn (string ".") (var "s")] $
    Logic.ifElse (Equality.gte (Lists.length (var "parts")) (int32 2))
      (Strings.intercalate (string ".") (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit (var "parts"))))
      (var "s")

-- | Extract the leading forall-bound parameter names from a type, returning
-- the remaining body.
extractTypeParams :: TTermDefinition (Type -> ([String], Type))
extractTypeParams = define "extractTypeParams" $
  doc "Peel off leading forall binders, returning the list of parameter names and the inner body type" $
  lambda "ty" $ cases _Type (var "ty") (Just (pair (list ([] :: [TTerm String])) (var "ty"))) [
    _Type_forall>>: "ft" ~> lets [
      "param">: unwrap _Name @@ (Core.forallTypeParameter $ var "ft"),
      "rest">: extractTypeParams @@ (Core.forallTypeBody $ var "ft")] $
      pair (Lists.cons (var "param") (Pairs.first $ var "rest")) (Pairs.second $ var "rest"),
    _Type_annotated>>: "at" ~>
      extractTypeParams @@ (Core.annotatedTypeBody $ var "at")]

-- | Does a name look like a Hydra-generated type variable (`t0`, `t1`, ...)?
isTypeVarLike :: TTermDefinition (String -> Bool)
isTypeVarLike = define "isTypeVarLike" $
  doc "Return True if the string is of the form `t<digits>` with at least one digit" $
  lambda "s" $ lets [
    "chars">: Strings.toList (var "s")] $
    Maybes.fromMaybe (boolean False) (Maybes.map
      (lambda "p" $ lets [
        "firstCh">: Pairs.first (var "p"),
        "rest">: Pairs.second (var "p")] $
        Logic.ifElse (Logic.not (Equality.equal (var "firstCh") (int32 116)))
          (boolean False)
          (Logic.and (Logic.not (Lists.null (var "rest")))
                    (Lists.foldl
                      (lambdas ["acc", "c"] $ Logic.and (var "acc")
                        (Logic.and (Equality.gte (var "c") (int32 48))
                                   (Equality.lte (var "c") (int32 57))))
                      (boolean True)
                      (var "rest"))))
      (Lists.uncons (var "chars")))

-- | Take the last dot-separated segment of a qualified Hydra name and sanitise
-- it against Coq's stripped-reserved-words list.
localName :: TTermDefinition (String -> String)
localName = define "localName" $
  doc "Return the last dot-separated segment of a qualified Hydra name, sanitised via `sanitize`" $
  lambda "s" $ lets [
    "parts">: Strings.splitOn (string ".") (var "s"),
    "raw">: Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts"))] $
    sanitize @@ var "raw"

-- | Sanitise a name that appears in a stripped-local reference position: if it
-- collides with a Coq reserved word, append an underscore.
sanitize :: TTermDefinition (String -> String)
sanitize = define "sanitize" $
  doc "Escape a stripped local name against Coq's stripped reserved-words set" $
  lambda "s" $ Formatting.escapeWithUnderscore @@ CoqLanguage.coqStrippedReservedWords @@ var "s"

-- | Build a map from union-type name to the number of its constructors. Used
-- by the encoder's union-eliminator to decide whether a match is exhaustive.
buildConstructorCounts :: TTermDefinition ([(String, Type)] -> M.Map String I.Int32)
buildConstructorCounts = define "buildConstructorCounts" $
  doc "Build a map from each union-type definition's name to its constructor count" $
  lambda "defs" $ Maps.fromList $ Lists.concat $
    Lists.map (lambda "nt" $ lets [
      "name">: Pairs.first (var "nt"),
      "ty">: Pairs.second (var "nt"),
      "extracted">: extractTypeParams @@ var "ty",
      "bodyTy">: Pairs.second (var "extracted")] $
      cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm (String, I.Int32)])) [
        _Type_union>>: "fields" ~>
          list [pair (var "name") (Lists.length $ var "fields")]])
    (var "defs")

-- | Return a singleton set containing the given Name's raw string iff it begins
-- with `"hydra."`; empty set otherwise.
qualifiedFromName :: TTermDefinition (Name -> S.Set String)
qualifiedFromName = define "qualifiedFromName" $
  doc "Wrap a Hydra Name as a singleton set of its raw string, iff it is a qualified (hydra.*) reference" $
  lambda "n" $ lets [
    "raw">: unwrap _Name @@ var "n",
    "parts">: Strings.splitOn (string ".") (var "raw")] $
    Logic.ifElse (Logic.and
        (Equality.gte (Lists.length (var "parts")) (int32 2))
        (Equality.equal (Maybes.fromMaybe (string "") (Lists.maybeHead (var "parts"))) (string "hydra")))
      (Sets.singleton (var "raw"))
      (Sets.empty :: TTerm (S.Set String))

-- | Walk a Hydra Type and collect every Name occurrence that begins with
-- `"hydra."` (a qualified cross-module reference).
collectQualifiedNamesInType :: TTermDefinition (Type -> S.Set String)
collectQualifiedNamesInType = define "collectQualifiedNamesInType" $
  doc "Collect the set of qualified (hydra.*) Name strings that a Hydra Type references" $
  lambda "ty" $ cases _Type (var "ty") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Type_annotated>>: "at" ~>
      collectQualifiedNamesInType @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~> Sets.union
      (collectQualifiedNamesInType @@ (Core.applicationTypeFunction $ var "app"))
      (collectQualifiedNamesInType @@ (Core.applicationTypeArgument $ var "app")),
    _Type_either>>: "et" ~> Sets.union
      (collectQualifiedNamesInType @@ (Core.eitherTypeLeft $ var "et"))
      (collectQualifiedNamesInType @@ (Core.eitherTypeRight $ var "et")),
    _Type_forall>>: "ft" ~>
      collectQualifiedNamesInType @@ (Core.forallTypeBody $ var "ft"),
    _Type_function>>: "ft" ~> Sets.union
      (collectQualifiedNamesInType @@ (Core.functionTypeDomain $ var "ft"))
      (collectQualifiedNamesInType @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~> collectQualifiedNamesInType @@ var "t",
    _Type_map>>: "mt" ~> Sets.union
      (collectQualifiedNamesInType @@ (Core.mapTypeKeys $ var "mt"))
      (collectQualifiedNamesInType @@ (Core.mapTypeValues $ var "mt")),
    _Type_maybe>>: "t" ~> collectQualifiedNamesInType @@ var "t",
    _Type_pair>>: "pt" ~> Sets.union
      (collectQualifiedNamesInType @@ (Core.pairTypeFirst $ var "pt"))
      (collectQualifiedNamesInType @@ (Core.pairTypeSecond $ var "pt")),
    _Type_record>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ collectQualifiedNamesInType @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_set>>: "t" ~> collectQualifiedNamesInType @@ var "t",
    _Type_union>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ collectQualifiedNamesInType @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_variable>>: "n" ~> qualifiedFromName @@ var "n",
    _Type_wrap>>: "wt" ~> collectQualifiedNamesInType @@ var "wt"]

-- | Walk a Hydra Term and collect every qualified Name occurrence. Also
-- recurses into type annotations and type applications so that qualified
-- type-level references are picked up.
collectQualifiedNamesInTerm :: TTermDefinition (Term -> S.Set String)
collectQualifiedNamesInTerm = define "collectQualifiedNamesInTerm" $
  doc "Collect the set of qualified (hydra.*) Name strings that a Hydra Term references" $
  lambda "tm" $ cases _Term (var "tm") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Term_annotated>>: "at" ~>
      collectQualifiedNamesInTerm @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~> Sets.union
      (collectQualifiedNamesInTerm @@ (Core.applicationFunction $ var "app"))
      (collectQualifiedNamesInTerm @@ (Core.applicationArgument $ var "app")),
    _Term_cases>>: "cs" ~> Sets.union
      (qualifiedFromName @@ (Core.caseStatementTypeName $ var "cs"))
      (Sets.union
        (Sets.unions $ Lists.map
          (lambda "f" $ collectQualifiedNamesInTerm @@ (Core.fieldTerm $ var "f"))
          (Core.caseStatementCases $ var "cs"))
        (Maybes.maybe (Sets.empty :: TTerm (S.Set String))
          (lambda "d" $ collectQualifiedNamesInTerm @@ var "d")
          (Core.caseStatementDefault $ var "cs"))),
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ collectQualifiedNamesInTerm @@ var "l")
      (lambda "r" $ collectQualifiedNamesInTerm @@ var "r")
      (var "e"),
    _Term_inject>>: "inj" ~> Sets.union
      (qualifiedFromName @@ (Core.injectionTypeName $ var "inj"))
      (collectQualifiedNamesInTerm @@ (Core.fieldTerm $ Core.injectionField $ var "inj")),
    _Term_lambda>>: "lam" ~> Sets.union
      (Maybes.maybe (Sets.empty :: TTerm (S.Set String))
        (lambda "domTy" $ collectQualifiedNamesInType @@ var "domTy")
        (Core.lambdaDomain $ var "lam"))
      (collectQualifiedNamesInTerm @@ (Core.lambdaBody $ var "lam")),
    _Term_let>>: "lt" ~> Sets.union
      (Sets.unions $ Lists.map
        (lambda "b" $ collectQualifiedNamesInTerm @@ (Core.bindingTerm $ var "b"))
        (Core.letBindings $ var "lt"))
      (collectQualifiedNamesInTerm @@ (Core.letBody $ var "lt")),
    _Term_list>>: "xs" ~> Sets.unions $
      Lists.map (lambda "el" $ collectQualifiedNamesInTerm @@ var "el") (var "xs"),
    _Term_maybe>>: "mt" ~> Maybes.maybe (Sets.empty :: TTerm (S.Set String))
      (lambda "el" $ collectQualifiedNamesInTerm @@ var "el")
      (var "mt"),
    _Term_pair>>: "p" ~> Sets.union
      (collectQualifiedNamesInTerm @@ Pairs.first (var "p"))
      (collectQualifiedNamesInTerm @@ Pairs.second (var "p")),
    _Term_record>>: "r" ~> Sets.union
      (qualifiedFromName @@ (Core.recordTypeName $ var "r"))
      (Sets.unions $ Lists.map
        (lambda "f" $ collectQualifiedNamesInTerm @@ (Core.fieldTerm $ var "f"))
        (Core.recordFields $ var "r")),
    _Term_typeApplication>>: "ta" ~> Sets.union
      (collectQualifiedNamesInTerm @@ (Core.typeApplicationTermBody $ var "ta"))
      (collectQualifiedNamesInType @@ (Core.typeApplicationTermType $ var "ta")),
    _Term_typeLambda>>: "tl" ~>
      collectQualifiedNamesInTerm @@ (Core.typeLambdaBody $ var "tl"),
    _Term_variable>>: "n" ~> qualifiedFromName @@ var "n",
    _Term_wrap>>: "wt" ~>
      collectQualifiedNamesInTerm @@ (Core.wrappedTermBody $ var "wt")]

-- | Walk a Hydra TypeScheme, collecting qualified Names from its body type.
collectQualifiedNamesInTypeScheme :: TTermDefinition (TypeScheme -> S.Set String)
collectQualifiedNamesInTypeScheme = define "collectQualifiedNamesInTypeScheme" $
  doc "Collect qualified (hydra.*) Name strings from a TypeScheme's body, after stripping forall binders" $
  lambda "ts" $ lets [
    "extracted">: extractTypeParams @@ (Core.typeSchemeBody $ var "ts"),
    "body">: Pairs.second (var "extracted")] $
    collectQualifiedNamesInType @@ var "body"

-- | Extract the set of local type names (sanitised-local forms) that a Hydra
-- Type refers to, filtered by a provided set of locally-defined names. Used to
-- build the dependency graph for SCC-based mutual-recursion detection in type
-- definitions.
typeRefs :: TTermDefinition (S.Set String -> Type -> S.Set String)
typeRefs = define "typeRefs" $
  doc "Walk a Type and collect the local names it references, intersected with the given locally-defined names" $
  lambdas ["locals", "ty"] $ cases _Type (var "ty") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Type_annotated>>: "at" ~>
      typeRefs @@ var "locals" @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~> Sets.union
      (typeRefs @@ var "locals" @@ (Core.applicationTypeFunction $ var "app"))
      (typeRefs @@ var "locals" @@ (Core.applicationTypeArgument $ var "app")),
    _Type_either>>: "et" ~> Sets.union
      (typeRefs @@ var "locals" @@ (Core.eitherTypeLeft $ var "et"))
      (typeRefs @@ var "locals" @@ (Core.eitherTypeRight $ var "et")),
    _Type_forall>>: "ft" ~>
      typeRefs @@ var "locals" @@ (Core.forallTypeBody $ var "ft"),
    _Type_function>>: "ft" ~> Sets.union
      (typeRefs @@ var "locals" @@ (Core.functionTypeDomain $ var "ft"))
      (typeRefs @@ var "locals" @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~> typeRefs @@ var "locals" @@ var "t",
    _Type_map>>: "mt" ~> Sets.union
      (typeRefs @@ var "locals" @@ (Core.mapTypeKeys $ var "mt"))
      (typeRefs @@ var "locals" @@ (Core.mapTypeValues $ var "mt")),
    _Type_maybe>>: "t" ~> typeRefs @@ var "locals" @@ var "t",
    _Type_pair>>: "pt" ~> Sets.union
      (typeRefs @@ var "locals" @@ (Core.pairTypeFirst $ var "pt"))
      (typeRefs @@ var "locals" @@ (Core.pairTypeSecond $ var "pt")),
    _Type_record>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ typeRefs @@ var "locals" @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_set>>: "t" ~> typeRefs @@ var "locals" @@ var "t",
    _Type_union>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ typeRefs @@ var "locals" @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_variable>>: "n" ~> lets [
      "local">: localName @@ (unwrap _Name @@ var "n")] $
      Logic.ifElse (Sets.member (var "local") (var "locals"))
        (Sets.singleton (var "local"))
        (Sets.empty :: TTerm (S.Set String)),
    _Type_wrap>>: "wt" ~> typeRefs @@ var "locals" @@ var "wt"]

-- | Walk a Term collecting local name references, filtered by a provided set
-- of locally-defined names.
termRefs :: TTermDefinition (S.Set String -> Term -> S.Set String)
termRefs = define "termRefs" $
  doc "Walk a Term and collect the local names it references, intersected with the given locally-defined names" $
  lambdas ["locals", "tm"] $ cases _Term (var "tm") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Term_annotated>>: "at" ~>
      termRefs @@ var "locals" @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~> Sets.union
      (termRefs @@ var "locals" @@ (Core.applicationFunction $ var "app"))
      (termRefs @@ var "locals" @@ (Core.applicationArgument $ var "app")),
    _Term_cases>>: "cs" ~> Sets.union
      (Sets.unions $ Lists.map
        (lambda "f" $ termRefs @@ var "locals" @@ (Core.fieldTerm $ var "f"))
        (Core.caseStatementCases $ var "cs"))
      (Maybes.maybe (Sets.empty :: TTerm (S.Set String))
        (lambda "d" $ termRefs @@ var "locals" @@ var "d")
        (Core.caseStatementDefault $ var "cs")),
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ termRefs @@ var "locals" @@ var "l")
      (lambda "r" $ termRefs @@ var "locals" @@ var "r")
      (var "e"),
    _Term_inject>>: "inj" ~>
      termRefs @@ var "locals" @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
    _Term_lambda>>: "lam" ~>
      termRefs @@ var "locals" @@ (Core.lambdaBody $ var "lam"),
    _Term_let>>: "lt" ~> Sets.union
      (Sets.unions $ Lists.map
        (lambda "b" $ termRefs @@ var "locals" @@ (Core.bindingTerm $ var "b"))
        (Core.letBindings $ var "lt"))
      (termRefs @@ var "locals" @@ (Core.letBody $ var "lt")),
    _Term_list>>: "xs" ~> Sets.unions $
      Lists.map (lambda "el" $ termRefs @@ var "locals" @@ var "el") (var "xs"),
    _Term_maybe>>: "mt" ~> Maybes.maybe (Sets.empty :: TTerm (S.Set String))
      (lambda "el" $ termRefs @@ var "locals" @@ var "el")
      (var "mt"),
    _Term_pair>>: "p" ~> Sets.union
      (termRefs @@ var "locals" @@ Pairs.first (var "p"))
      (termRefs @@ var "locals" @@ Pairs.second (var "p")),
    _Term_record>>: "r" ~> Sets.unions $
      Lists.map
        (lambda "f" $ termRefs @@ var "locals" @@ (Core.fieldTerm $ var "f"))
        (Core.recordFields $ var "r"),
    _Term_typeApplication>>: "ta" ~>
      termRefs @@ var "locals" @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~>
      termRefs @@ var "locals" @@ (Core.typeLambdaBody $ var "tl"),
    _Term_variable>>: "n" ~> lets [
      "local">: localName @@ (unwrap _Name @@ var "n")] $
      Logic.ifElse (Sets.member (var "local") (var "locals"))
        (Sets.singleton (var "local"))
        (Sets.empty :: TTerm (S.Set String)),
    _Term_wrap>>: "wt" ~>
      termRefs @@ var "locals" @@ (Core.wrappedTermBody $ var "wt")]

-- | Collect the dependency namespaces of a Hydra Module: the union of its
-- type and term dependency namespaces, minus the module's own namespace,
-- deduplicated while preserving first-occurrence order.
moduleDependencies :: TTermDefinition (Module -> [String])
moduleDependencies = define "moduleDependencies" $
  doc "Return the deduplicated list of dependency namespace strings for a Module, excluding its own namespace" $
  lambda "m" $ lets [
    "typeDeps">: Lists.map (lambda "ns" $ Packaging.unNamespace (var "ns")) (Packaging.moduleTypeDependencies (var "m")),
    "termDeps">: Lists.map (lambda "ns" $ Packaging.unNamespace (var "ns")) (Packaging.moduleTermDependencies (var "m")),
    "ownNs">: Packaging.unNamespace (Packaging.moduleNamespace (var "m")),
    "allDeps">: Lists.concat2 (var "typeDeps") (var "termDeps"),
    "filtered">: Lists.filter (lambda "s" $ Logic.not (Equality.equal (var "s") (var "ownNs"))) (var "allDeps")] $
    (Lists.nub :: TTerm [String] -> TTerm [String]) (var "filtered")

-- | Does a Type reference any type variable whose sanitized local name
-- belongs to the given set of group names? Used by the mutual-inductive
-- positivity analysis.
typeContainsGroupRef :: TTermDefinition (S.Set String -> Type -> Bool)
typeContainsGroupRef = define "typeContainsGroupRef" $
  doc "Return True if the Type mentions any type variable whose local name is in the given set" $
  lambdas ["groupNames", "ty"] $ cases _Type (var "ty") (Just (boolean False)) [
    _Type_annotated>>: "at" ~>
      typeContainsGroupRef @@ var "groupNames" @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.applicationTypeFunction $ var "app"))
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.applicationTypeArgument $ var "app")),
    _Type_either>>: "et" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.eitherTypeLeft $ var "et"))
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.eitherTypeRight $ var "et")),
    _Type_forall>>: "ft" ~>
      typeContainsGroupRef @@ var "groupNames" @@ (Core.forallTypeBody $ var "ft"),
    _Type_function>>: "ft" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.functionTypeDomain $ var "ft"))
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~> typeContainsGroupRef @@ var "groupNames" @@ var "t",
    _Type_map>>: "mt" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.mapTypeKeys $ var "mt"))
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.mapTypeValues $ var "mt")),
    _Type_maybe>>: "t" ~> typeContainsGroupRef @@ var "groupNames" @@ var "t",
    _Type_pair>>: "pt" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.pairTypeFirst $ var "pt"))
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.pairTypeSecond $ var "pt")),
    _Type_set>>: "t" ~> typeContainsGroupRef @@ var "groupNames" @@ var "t",
    _Type_variable>>: "n" ~>
      Sets.member (localName @@ (unwrap _Name @@ var "n")) (var "groupNames"),
    _Type_wrap>>: "wt" ~> typeContainsGroupRef @@ var "groupNames" @@ var "wt"]

-- | Does a field's type cause a strict-positivity violation with respect to
-- the given group? A violation occurs when any group member appears in the
-- domain of a function type, recursively descending through annotations,
-- foralls, wrappers, and function codomains.
fieldCausesPositivityIssue :: TTermDefinition (S.Set String -> Type -> Bool)
fieldCausesPositivityIssue = define "fieldCausesPositivityIssue" $
  doc "Return True if the field type contains a function whose domain mentions a group member" $
  lambdas ["groupNames", "fty"] $ cases _Type (var "fty") (Just (boolean False)) [
    _Type_function>>: "ft" ~> Logic.or
      (typeContainsGroupRef @@ var "groupNames" @@ (Core.functionTypeDomain $ var "ft"))
      (fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_annotated>>: "at" ~>
      fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.annotatedTypeBody $ var "at"),
    _Type_forall>>: "ft" ~>
      fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.forallTypeBody $ var "ft"),
    _Type_wrap>>: "wt" ~>
      fieldCausesPositivityIssue @@ var "groupNames" @@ var "wt"]

-- | Does a mutual group of type definitions contain any field that causes a
-- strict-positivity violation?
hasPositivityIssue :: TTermDefinition (S.Set String -> [(String, Type)] -> Bool)
hasPositivityIssue = define "hasPositivityIssue" $
  doc "Return True if any definition in the group has a record/union field whose type causes a positivity violation" $
  lambdas ["groupNames", "defs"] $
    Lists.foldl
      (lambdas ["acc", "nt"] $ Logic.or (var "acc") $ lets [
        "ty">: Pairs.second (var "nt"),
        "extracted">: extractTypeParams @@ var "ty",
        "bodyTy">: Pairs.second (var "extracted")] $
        cases _Type (var "bodyTy") (Just (boolean False)) [
          _Type_record>>: "fields" ~>
            Lists.foldl
              (lambdas ["acc2", "f"] $ Logic.or (var "acc2")
                (fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.fieldTypeType $ var "f")))
              (boolean False)
              (var "fields"),
          _Type_union>>: "fields" ~>
            Lists.foldl
              (lambdas ["acc2", "f"] $ Logic.or (var "acc2")
                (fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.fieldTypeType $ var "f")))
              (boolean False)
              (var "fields")])
      (boolean False)
      (var "defs")

-- | Replace positivity-violating fields with `unit`, re-wrapping any forall
-- binders. Used to allow otherwise mutually recursive Coq Inductive groups to
-- type-check. The generated accessor becomes a dummy that callers replace
-- with `hydra_unreachable`; see `collectSanitizedAccessors`.
sanitizePositivity :: TTermDefinition (S.Set String -> Type -> Type)
sanitizePositivity = define "sanitizePositivity" $
  doc "Rewrite a Type, replacing offending record/union fields with TypeUnit and restoring forall binders" $
  lambdas ["groupNames", "ty"] $ lets [
    "extracted">: extractTypeParams @@ var "ty",
    "params">: Pairs.first (var "extracted"),
    "bodyTy">: Pairs.second (var "extracted"),
    "sanitizeField">: lambda "f" $
      Logic.ifElse (fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.fieldTypeType $ var "f"))
        (Core.fieldType (Core.fieldTypeName $ var "f") (Core.typeUnit))
        (var "f"),
    "sanitized">: cases _Type (var "bodyTy") (Just (var "bodyTy")) [
      _Type_record>>: "fields" ~>
        Core.typeRecord (Lists.map (var "sanitizeField") (var "fields")),
      _Type_union>>: "fields" ~>
        Core.typeUnion (Lists.map (var "sanitizeField") (var "fields"))]] $
    Lists.foldr
      (lambdas ["p", "t"] $ Core.typeForall (Core.forallType (wrap _Name (var "p")) (var "t")))
      (var "sanitized")
      (var "params")

-- | Collect the names of accessor functions generated for fields that were
-- sanitized away due to positivity issues. These names look like
-- `typeName_fieldName` (with the type name decapitalized and the field name
-- sanitized against Coq reserved words). The Coq encoder replaces references
-- to these accessors with `hydra_unreachable`.
collectSanitizedAccessors :: TTermDefinition ([(Bool, [(String, Type)])] -> S.Set String)
collectSanitizedAccessors = define "collectSanitizedAccessors" $
  doc "Return the set of decapitalized, sanitized accessor names whose fields were replaced with unit due to positivity issues" $
  lambda "typeGroups" $ Sets.fromList $ Lists.concat $
    Lists.map (lambda "group" $ lets [
      "defs">: Pairs.second (var "group"),
      "groupNames">: Sets.fromList $ Lists.map (lambda "nt" $ Pairs.first (var "nt")) (var "defs")] $
      Logic.ifElse (hasPositivityIssue @@ var "groupNames" @@ var "defs")
        (Lists.concat $ Lists.map (lambda "nt" $ lets [
          "typeName">: Pairs.first (var "nt"),
          "ty">: Pairs.second (var "nt"),
          "extracted">: extractTypeParams @@ var "ty",
          "bodyTy">: Pairs.second (var "extracted")] $
          cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm String])) [
            _Type_record>>: "fields" ~>
              Maybes.cat $ Lists.map (lambda "f" $
                Logic.ifElse (fieldCausesPositivityIssue @@ var "groupNames" @@ (Core.fieldTypeType $ var "f"))
                  (Phantoms.just $ Strings.cat (list [
                    Formatting.decapitalize @@ var "typeName",
                    string "_",
                    sanitize @@ (localName @@ (unwrap _Name @@ (Core.fieldTypeName $ var "f")))]))
                  (Phantoms.nothing :: TTerm (Maybe String)))
                (var "fields")]) (var "defs"))
        (list ([] :: [TTerm String])))
      (var "typeGroups")

-- | Take the last dot-separated segment of a qualified Hydra name, without
-- sanitizing it against Coq reserved words. Used when the raw field name is
-- needed as a map key; contrast with `localName` which sanitizes.
localNameRaw :: TTermDefinition (String -> String)
localNameRaw = define "localNameRaw" $
  doc "Return the last dot-separated segment of a qualified Hydra name, unsanitized" $
  lambda "s" $ lets [
    "parts">: Strings.splitOn (string ".") (var "s")] $
    Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts"))

-- | Build a map from (qualifiedTypeName, bareFieldName) to a prefixed field
-- name like `typeName_fieldName`. Used to disambiguate Coq record accessor
-- names across a multi-module kernel where plain Hydra field names collide.
buildFieldMapping :: TTermDefinition ([Module] -> M.Map (String, String) String)
buildFieldMapping = define "buildFieldMapping" $
  doc "Build a map keyed by (qualifiedTypeName, rawFieldName) producing the prefixed Coq accessor name" $
  lambda "modules" $ Maps.fromList $ Lists.concat $
    Lists.map (lambda "m" $ Lists.concat $
      Lists.map (lambda "def_" $
        cases _Definition (var "def_") (Just $ list ([] :: [TTerm ((String, String), String)])) [
          _Definition_type>>: "td" ~> lets [
            "qname">: unwrap _Name @@ (Packaging.typeDefinitionName $ var "td"),
            "tname">: localName @@ var "qname",
            "ty">: Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme $ var "td"),
            "extracted">: extractTypeParams @@ var "ty",
            "bodyTy">: Pairs.second (var "extracted")] $
            cases _Type (var "bodyTy") (Just $ list ([] :: [TTerm ((String, String), String)])) [
              _Type_record>>: "fields" ~> Lists.map (lambda "ft" $ lets [
                "rawFn">: localNameRaw @@ (unwrap _Name @@ (Core.fieldTypeName $ var "ft")),
                "fn">: sanitize @@ var "rawFn",
                "prefixed">: Strings.cat (list [
                  Formatting.decapitalize @@ var "tname",
                  string "_",
                  var "fn"])] $
                pair (pair (var "qname") (var "rawFn")) (var "prefixed")) (var "fields")]])
        (Packaging.moduleDefinitions $ var "m"))
      (var "modules")

-- | Rewrite all TermProject nodes in a term, replacing each field name with
-- its prefixed form from the supplied mapping. Leaves unmapped projections
-- untouched. Recurses over all subterms via `Rewriting.rewriteTerm`.
rewriteTermFields :: TTermDefinition (M.Map (String, String) String -> Term -> Term)
rewriteTermFields = define "rewriteTermFields" $
  doc "Replace field names in TermProject nodes using the given (typeName, rawFieldName) -> prefixedName map" $
  lambdas ["fm", "term0"] $ lets [
    "rewrite">: lambdas ["recurse", "term"] $
      cases _Term (var "term") (Just $ var "recurse" @@ var "term") [
        _Term_project>>: "p" ~> lets [
          "tname">: unwrap _Name @@ (Core.projectionTypeName $ var "p"),
          "rawFn">: localNameRaw @@ (unwrap _Name @@ (Core.projectionField $ var "p")),
          "key">: pair (var "tname") (var "rawFn"),
          "newFname">: Maybes.fromMaybe (Core.projectionField $ var "p")
            (Maybes.map (lambda "s" $ wrap _Name (var "s")) (Maps.lookup (var "key") (var "fm")))] $
          Core.termProject $ Core.projection
            (Core.projectionTypeName $ var "p")
            (var "newFname")]] $
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"

-- | Collect free type-variable-like names (e.g. `t0`, `t1`) from a Type.
-- Used by the inner-term transformations to detect type parameters that
-- escape their binders and must be rebound at a higher forall.
collectFreeTypeVarsInType :: TTermDefinition (Type -> S.Set String)
collectFreeTypeVarsInType = define "collectFreeTypeVarsInType" $
  doc "Collect names of type-variable-like references (t0, t1, ...) inside a Type" $
  lambda "ty" $ cases _Type (var "ty") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Type_annotated>>: "at" ~>
      collectFreeTypeVarsInType @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~> Sets.union
      (collectFreeTypeVarsInType @@ (Core.applicationTypeFunction $ var "app"))
      (collectFreeTypeVarsInType @@ (Core.applicationTypeArgument $ var "app")),
    _Type_forall>>: "ft" ~>
      collectFreeTypeVarsInType @@ (Core.forallTypeBody $ var "ft"),
    _Type_function>>: "ft" ~> Sets.union
      (collectFreeTypeVarsInType @@ (Core.functionTypeDomain $ var "ft"))
      (collectFreeTypeVarsInType @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~> collectFreeTypeVarsInType @@ var "t",
    _Type_map>>: "mt" ~> Sets.union
      (collectFreeTypeVarsInType @@ (Core.mapTypeKeys $ var "mt"))
      (collectFreeTypeVarsInType @@ (Core.mapTypeValues $ var "mt")),
    _Type_maybe>>: "t" ~> collectFreeTypeVarsInType @@ var "t",
    _Type_pair>>: "pt" ~> Sets.union
      (collectFreeTypeVarsInType @@ (Core.pairTypeFirst $ var "pt"))
      (collectFreeTypeVarsInType @@ (Core.pairTypeSecond $ var "pt")),
    _Type_record>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ collectFreeTypeVarsInType @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_set>>: "t" ~> collectFreeTypeVarsInType @@ var "t",
    _Type_union>>: "fields" ~> Sets.unions $
      Lists.map (lambda "f" $ collectFreeTypeVarsInType @@ (Core.fieldTypeType $ var "f")) (var "fields"),
    _Type_variable>>: "n" ~> lets [
      "nm">: unwrap _Name @@ var "n"] $
      Logic.ifElse (isTypeVarLike @@ var "nm")
        (Sets.singleton (var "nm"))
        (Sets.empty :: TTerm (S.Set String)),
    _Type_wrap>>: "wt" ~> collectFreeTypeVarsInType @@ var "wt"]

-- | Collect free type-variable-like names from a TypeScheme: both the
-- explicitly quantified variables (if they look like `t0`) and any such
-- references inside the body type.
collectFreeTypeVarsInTypeScheme :: TTermDefinition (TypeScheme -> S.Set String)
collectFreeTypeVarsInTypeScheme = define "collectFreeTypeVarsInTypeScheme" $
  doc "Collect type-variable-like names declared or referenced by a TypeScheme" $
  lambda "ts" $ lets [
    "explicit">: Sets.fromList $ Lists.map (lambda "n" $ unwrap _Name @@ var "n") $
      Lists.filter (lambda "n" $ isTypeVarLike @@ (unwrap _Name @@ var "n"))
        (Core.typeSchemeVariables $ var "ts")] $
    Sets.union (var "explicit") (collectFreeTypeVarsInType @@ (Core.typeSchemeBody $ var "ts"))

-- | Collect all free type-variable-like names referenced inside a Term,
-- recursing through lambda domains, type applications, let binding type
-- schemes, etc. Binding occurrences remove the bound name from the result
-- (both type lambdas and term lambdas whose parameter looks like `t<n>`).
collectFreeTypeVars :: TTermDefinition (Term -> S.Set String)
collectFreeTypeVars = define "collectFreeTypeVars" $
  doc "Collect the set of free type-variable-like names (t0, t1, ...) referenced anywhere inside a Term" $
  lambda "tm" $ cases _Term (var "tm") (Just (Sets.empty :: TTerm (S.Set String))) [
    _Term_annotated>>: "at" ~>
      collectFreeTypeVars @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~> Sets.union
      (collectFreeTypeVars @@ (Core.applicationFunction $ var "app"))
      (collectFreeTypeVars @@ (Core.applicationArgument $ var "app")),
    _Term_cases>>: "cs" ~> Sets.union
      (Maybes.maybe (Sets.empty :: TTerm (S.Set String))
        (lambda "d" $ collectFreeTypeVars @@ var "d")
        (Core.caseStatementDefault $ var "cs"))
      (Sets.unions $ Lists.map
        (lambda "f" $ collectFreeTypeVars @@ (Core.fieldTerm $ var "f"))
        (Core.caseStatementCases $ var "cs")),
    _Term_either>>: "e" ~> Eithers.either_
      (lambda "l" $ collectFreeTypeVars @@ var "l")
      (lambda "r" $ collectFreeTypeVars @@ var "r")
      (var "e"),
    _Term_inject>>: "inj" ~>
      collectFreeTypeVars @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
    _Term_lambda>>: "lam" ~> lets [
      "paramName">: unwrap _Name @@ (Core.lambdaParameter $ var "lam"),
      "domVars">: Maybes.maybe (Sets.empty :: TTerm (S.Set String))
        (lambda "dty" $ collectFreeTypeVarsInType @@ var "dty")
        (Core.lambdaDomain $ var "lam"),
      "bodyVars">: collectFreeTypeVars @@ (Core.lambdaBody $ var "lam"),
      "allVars">: Sets.union (var "domVars") (var "bodyVars")] $
      Logic.ifElse (isTypeVarLike @@ var "paramName")
        (Sets.delete (var "paramName") (var "allVars"))
        (var "allVars"),
    _Term_let>>: "lt" ~> lets [
      "bindVars">: Sets.unions $ Lists.map
        (lambda "b" $ Sets.union
          (collectFreeTypeVars @@ (Core.bindingTerm $ var "b"))
          (Maybes.maybe (Sets.empty :: TTerm (S.Set String))
            (lambda "sch" $ collectFreeTypeVarsInTypeScheme @@ var "sch")
            (Core.bindingTypeScheme $ var "b")))
        (Core.letBindings $ var "lt")] $
      Sets.union (var "bindVars") (collectFreeTypeVars @@ (Core.letBody $ var "lt")),
    _Term_list>>: "xs" ~> Sets.unions $
      Lists.map (lambda "el" $ collectFreeTypeVars @@ var "el") (var "xs"),
    _Term_maybe>>: "mt" ~> Maybes.maybe (Sets.empty :: TTerm (S.Set String))
      (lambda "el" $ collectFreeTypeVars @@ var "el") (var "mt"),
    _Term_pair>>: "p" ~> Sets.union
      (collectFreeTypeVars @@ Pairs.first (var "p"))
      (collectFreeTypeVars @@ Pairs.second (var "p")),
    _Term_record>>: "r" ~> Sets.unions $
      Lists.map
        (lambda "f" $ collectFreeTypeVars @@ (Core.fieldTerm $ var "f"))
        (Core.recordFields $ var "r"),
    _Term_set>>: "s" ~> Sets.unions $
      Lists.map (lambda "el" $ collectFreeTypeVars @@ var "el") (Sets.toList $ var "s"),
    _Term_typeApplication>>: "ta" ~>
      collectFreeTypeVars @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~>
      Sets.delete (unwrap _Name @@ (Core.typeLambdaParameter $ var "tl"))
        (collectFreeTypeVars @@ (Core.typeLambdaBody $ var "tl")),
    _Term_wrap>>: "wt" ~>
      collectFreeTypeVars @@ (Core.wrappedTermBody $ var "wt")]

-- | Does a Type reference any type-variable-like name (e.g. `t0`) that is
-- NOT in the given set of bound type variables?
hasUnboundTypeVar :: TTermDefinition (S.Set String -> Type -> Bool)
hasUnboundTypeVar = define "hasUnboundTypeVar" $
  doc "Return True if the type mentions a t<digits> variable not present in the given set" $
  lambdas ["bound", "ty"] $ cases _Type (var "ty") (Just (boolean False)) [
    _Type_annotated>>: "at" ~>
      hasUnboundTypeVar @@ var "bound" @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~> Logic.or
      (hasUnboundTypeVar @@ var "bound" @@ (Core.applicationTypeFunction $ var "app"))
      (hasUnboundTypeVar @@ var "bound" @@ (Core.applicationTypeArgument $ var "app")),
    _Type_function>>: "ft" ~> Logic.or
      (hasUnboundTypeVar @@ var "bound" @@ (Core.functionTypeDomain $ var "ft"))
      (hasUnboundTypeVar @@ var "bound" @@ (Core.functionTypeCodomain $ var "ft")),
    _Type_list>>: "t" ~> hasUnboundTypeVar @@ var "bound" @@ var "t",
    _Type_map>>: "mt" ~> Logic.or
      (hasUnboundTypeVar @@ var "bound" @@ (Core.mapTypeKeys $ var "mt"))
      (hasUnboundTypeVar @@ var "bound" @@ (Core.mapTypeValues $ var "mt")),
    _Type_maybe>>: "t" ~> hasUnboundTypeVar @@ var "bound" @@ var "t",
    _Type_pair>>: "pt" ~> Logic.or
      (hasUnboundTypeVar @@ var "bound" @@ (Core.pairTypeFirst $ var "pt"))
      (hasUnboundTypeVar @@ var "bound" @@ (Core.pairTypeSecond $ var "pt")),
    _Type_set>>: "t" ~> hasUnboundTypeVar @@ var "bound" @@ var "t",
    _Type_variable>>: "n" ~> lets [
      "nm">: unwrap _Name @@ var "n"] $
      Logic.and (isTypeVarLike @@ var "nm")
                (Logic.not (Sets.member (var "nm") (var "bound")))]

-- | Erase lambda domain types that reference type-variable-like names not
-- present in the current bound set. These spurious domains are artifacts of
-- Hydra's generic type inference; erasing them lets Coq reinfer a concrete
-- domain. Term lambdas whose domain is `TypeVariable "Type"` introduce a
-- fresh type binder and extend the bound set before recursing into the body.
eraseUnboundTypeVarDomains :: TTermDefinition (S.Set String -> Term -> Term)
eraseUnboundTypeVarDomains = define "eraseUnboundTypeVarDomains" $
  doc "Erase lambda domain annotations referencing unbound type variables; recurse under new type binders" $
  "initialBound" ~> "term0" ~>
  "eraseIfUnbound" <~ ("bound" ~> "mdom" ~>
    Maybes.maybe (Phantoms.nothing :: TTerm (Maybe Type))
      ("ty" ~> Logic.ifElse (hasUnboundTypeVar @@ var "bound" @@ var "ty")
        (Phantoms.nothing :: TTerm (Maybe Type))
        (Phantoms.just $ var "ty"))
      (var "mdom")) $
  "f" <~ ("recurse" ~> "bound" ~> "term" ~>
    cases _Term (var "term") (Just $ var "recurse" @@ var "bound" @@ var "term") [
      _Term_lambda>>: "lam" ~>
        "paramName" <~ (unwrap _Name @@ (Core.lambdaParameter $ var "lam")) $
        "dom" <~ (Core.lambdaDomain $ var "lam") $
        "isTypeParam" <~ Maybes.maybe (boolean False)
          ("d" ~> cases _Type (var "d") (Just (boolean False)) [
            _Type_variable>>: "v" ~>
              Equality.equal (unwrap _Name @@ var "v") (string "Type")])
          (var "dom") $
        "bound2" <~ Logic.ifElse
          (Logic.and (var "isTypeParam") (isTypeVarLike @@ var "paramName"))
          (Sets.insert (var "paramName") (var "bound"))
          (var "bound") $
        Core.termLambda $ Core.lambda
          (Core.lambdaParameter $ var "lam")
          (var "eraseIfUnbound" @@ var "bound" @@ var "dom")
          (var "f" @@ var "recurse" @@ var "bound2" @@ (Core.lambdaBody $ var "lam"))]) $
  Rewriting.rewriteTermWithContext @@ var "f" @@ var "initialBound" @@ var "term0"

-- | Is this Term a TermTypeLambda (after peeling TermAnnotated wrappers)?
-- Used to detect let-bindings whose body starts with a type lambda so that
-- the binding gets converted into a polymorphic value at Coq emission time.
isTypeLambdaTerm :: TTermDefinition (Term -> Bool)
isTypeLambdaTerm = define "isTypeLambdaTerm" $
  doc "Return True if a Term (possibly under TermAnnotated wrappers) is a TermTypeLambda" $
  lambda "tm" $ cases _Term (var "tm") (Just (boolean False)) [
    _Term_annotated>>: "at" ~>
      isTypeLambdaTerm @@ (Core.annotatedTermBody $ var "at"),
    _Term_typeLambda>>: constant (boolean True)]

-- | Does a type application's ultimate target (after peeling TermAnnotated
-- and nested TermTypeApplication wrappers) resolve to a variable in the
-- given poly-names set?
targetsPolyName :: TTermDefinition (S.Set String -> Term -> Bool)
targetsPolyName = define "targetsPolyName" $
  doc "Return True if the innermost target of a (possibly nested) type application is a poly-converted local name" $
  lambdas ["polyNames", "tm"] $ cases _Term (var "tm") (Just (boolean False)) [
    _Term_variable>>: "v" ~>
      Sets.member (unwrap _Name @@ var "v") (var "polyNames"),
    _Term_typeApplication>>: "ta" ~>
      targetsPolyName @@ var "polyNames" @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_annotated>>: "at" ~>
      targetsPolyName @@ var "polyNames" @@ (Core.annotatedTermBody $ var "at")]

-- | Convert a Hydra Type into a Term that can be passed as an explicit type
-- argument in Coq. For aggregate types (records, unions, functions),
-- function types used as type args, and literal/unit, falls back to the
-- `unit` variable, since these are rarely used as explicit type parameters.
-- TypeForall binders are stripped; TypeAnnotated passes through.
typeToTerm :: TTermDefinition (Type -> Term)
typeToTerm = define "typeToTerm" $
  doc "Convert a Hydra Type to a placeholder Term for use as an explicit Coq type argument. Coq-builtin type constructors are marked with a `Coq.` prefix so the encoder can emit them raw without going through sanitizeVar, which would clash with user-level lambda parameters of the same name (e.g. `list` -> `list_`)." $
  lambda "ty" $ cases _Type (var "ty") (Just $ Core.termVariable (wrap _Name (string "Coq.unit"))) [
    _Type_variable>>: "v" ~> Core.termVariable $ var "v",
    _Type_list>>: "t" ~> Core.termApplication $ Core.application
      (Core.termVariable (wrap _Name (string "Coq.list"))) (typeToTerm @@ var "t"),
    _Type_maybe>>: "t" ~> Core.termApplication $ Core.application
      (Core.termVariable (wrap _Name (string "Coq.option"))) (typeToTerm @@ var "t"),
    _Type_set>>: "t" ~> Core.termApplication $ Core.application
      (Core.termVariable (wrap _Name (string "Coq.list"))) (typeToTerm @@ var "t"),
    _Type_application>>: "at" ~> Core.termApplication $ Core.application
      (typeToTerm @@ (Core.applicationTypeFunction $ var "at"))
      (typeToTerm @@ (Core.applicationTypeArgument $ var "at")),
    _Type_function>>: constant (Core.termVariable $ wrap _Name (string "Coq.unit")),
    _Type_pair>>: "pt" ~> Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable (wrap _Name (string "Coq.prod")))
        (typeToTerm @@ (Core.pairTypeFirst $ var "pt")))
      (typeToTerm @@ (Core.pairTypeSecond $ var "pt")),
    _Type_map>>: "mt" ~> Core.termApplication $ Core.application
      (Core.termVariable (wrap _Name (string "Coq.list")))
      (Core.termApplication $ Core.application
        (Core.termVariable (wrap _Name (string "Coq.prod")))
        (typeToTerm @@ (Core.mapTypeKeys $ var "mt"))),
    _Type_unit>>: constant (Core.termVariable $ wrap _Name (string "Coq.unit")),
    _Type_literal>>: constant (Core.termVariable $ wrap _Name (string "Coq.unit")),
    _Type_either>>: "et" ~> Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable (wrap _Name (string "Coq.sum")))
        (typeToTerm @@ (Core.eitherTypeLeft $ var "et")))
      (typeToTerm @@ (Core.eitherTypeRight $ var "et")),
    _Type_record>>: constant (Core.termVariable $ wrap _Name (string "Coq.unit")),
    _Type_union>>: constant (Core.termVariable $ wrap _Name (string "Coq.unit")),
    _Type_wrap>>: "wt" ~> typeToTerm @@ var "wt",
    _Type_annotated>>: "at" ~> typeToTerm @@ (Core.annotatedTypeBody $ var "at"),
    _Type_forall>>: "ft" ~> typeToTerm @@ (Core.forallTypeBody $ var "ft"),
    _Type_void>>: constant (Core.termVariable $ wrap _Name (string "Coq.Empty_set"))]

-- | Normalize inner TermTypeLambda nodes inside a term, rewriting them as
-- regular term lambdas whose domain is `Type`. At each TermLet node, any
-- binding whose term starts with a TermTypeLambda is recorded in a
-- poly-names context and its bindingType cleared. Subsequent type
-- applications targeting those names get rewritten into term applications
-- (with `typeToTerm` producing the type argument); other type applications
-- are erased since their targets are treated as implicitly polymorphic.
-- The outer TermTypeLambda chain (matching the definition's `typeVars`) is
-- stripped and rebuilt around the transformed body so it stays intact.
normalizeInnerTypeLambdas :: TTermDefinition (Term -> Term)
normalizeInnerTypeLambdas = define "normalizeInnerTypeLambdas" $
  doc "Rewrite inner TermTypeLambda nodes and type applications so that polymorphic helpers work under Coq's erasure-based encoding" $
  "term" ~>
  -- Strip outer TermTypeLambda chain: returns (params, innerBody).
  "stripTypeLambdas" <~ ("tm" ~>
    cases _Term (var "tm") (Just $ pair (list ([] :: [TTerm String])) (var "tm")) [
      _Term_typeLambda>>: "tl" ~>
        "rest" <~ (var "stripTypeLambdas" @@ (Core.typeLambdaBody $ var "tl")) $
        pair
          (Lists.cons (unwrap _Name @@ (Core.typeLambdaParameter $ var "tl"))
                      (Pairs.first $ var "rest"))
          (Pairs.second $ var "rest")]) $
  -- Rebuild a TermTypeLambda chain from a list of param names wrapped around body.
  "rebuildTypeLambdas" <~ ("params" ~> "body" ~>
    Lists.foldr
      ("p" ~> "acc" ~> Core.termTypeLambda $ Core.typeLambda (wrap _Name (var "p")) (var "acc"))
      (var "body")
      (var "params")) $
  -- Custom rewriter: threads the set of "poly-converted" let-bound names.
  "f" <~ ("recurse" ~> "polyNames" ~> "tm" ~>
    cases _Term (var "tm") (Just $ var "recurse" @@ var "polyNames" @@ var "tm") [
      _Term_let>>: "lt" ~>
        -- Newly converted names: bindings whose term is (transitively) a TermTypeLambda.
        "newPoly" <~ Sets.fromList (Maybes.cat $ Lists.map
          ("b" ~> Logic.ifElse (isTypeLambdaTerm @@ (Core.bindingTerm $ var "b"))
            (Phantoms.just $ unwrap _Name @@ (Core.bindingName $ var "b"))
            (Phantoms.nothing :: TTerm (Maybe String)))
          (Core.letBindings $ var "lt")) $
        "polyNames2" <~ Sets.union (var "polyNames") (var "newPoly") $
        Core.termLet $ Core.let_
          (Lists.map ("b" ~> Core.binding
            (Core.bindingName $ var "b")
            (var "f" @@ var "recurse" @@ var "polyNames2" @@ (Core.bindingTerm $ var "b"))
            (Logic.ifElse (isTypeLambdaTerm @@ (Core.bindingTerm $ var "b"))
              (Phantoms.nothing :: TTerm (Maybe TypeScheme))
              (Core.bindingTypeScheme $ var "b")))
            (Core.letBindings $ var "lt"))
          (var "f" @@ var "recurse" @@ var "polyNames2" @@ (Core.letBody $ var "lt")),
      _Term_typeLambda>>: "tl" ~>
        -- Inner type lambda (outer chain has already been stripped): convert to a
        -- plain term lambda over the "Type" type variable, then reprocess.
        var "f" @@ var "recurse" @@ var "polyNames" @@ (Core.termLambda $ Core.lambda
          (Core.typeLambdaParameter $ var "tl")
          (Phantoms.just $ Core.typeVariable (wrap _Name (string "Type")))
          (Core.typeLambdaBody $ var "tl")),
      _Term_typeApplication>>: "ta" ~>
        "body" <~ (Core.typeApplicationTermBody $ var "ta") $
        "ttype" <~ (Core.typeApplicationTermType $ var "ta") $
        Logic.ifElse (targetsPolyName @@ var "polyNames" @@ var "body")
          -- Poly target: reify the type argument as a term application.
          (var "f" @@ var "recurse" @@ var "polyNames" @@ (Core.termApplication $ Core.application
            (var "body") (typeToTerm @@ var "ttype")))
          -- Non-poly target (library / imported): erase the type application entirely.
          (var "f" @@ var "recurse" @@ var "polyNames" @@ var "body")]) $
  -- Entry point: strip outer chain, transform body with empty polyNames, rebuild chain.
  "stripped" <~ var "stripTypeLambdas" @@ var "term" $
  "outerParams" <~ Pairs.first (var "stripped") $
  "body0" <~ Pairs.second (var "stripped") $
  Logic.ifElse (Lists.null (var "outerParams"))
    (var "term")
    (var "rebuildTypeLambdas" @@ var "outerParams" @@
      (Rewriting.rewriteTermWithContext @@ var "f" @@ (Sets.empty :: TTerm (S.Set String)) @@ var "body0"))

-- | Peel off consecutive TermLet wrappers from a term, returning the flat
-- list of all their bindings (in source order) and the final innermost body.
-- Hydra emits nested single-binding lets where Haskell's mutual let would
-- have appeared, and this helper recovers the flat binding group so that
-- SCC-based reordering can be applied to it.
collectLetBindings :: TTermDefinition (Term -> ([Binding], Term))
collectLetBindings = define "collectLetBindings" $
  doc "Flatten consecutive TermLet wrappers into (bindings, innermostBody)" $
  lambda "tm" $ cases _Term (var "tm") (Just $ pair (list ([] :: [TTerm Binding])) (var "tm")) [
    _Term_let>>: "lt" ~> lets [
      "rest">: collectLetBindings @@ (Core.letBody $ var "lt")] $
      pair
        (Lists.concat2 (Core.letBindings $ var "lt") (Pairs.first $ var "rest"))
        (Pairs.second $ var "rest")]

-- | Wrap each binding in its own nested TermLet, producing a sequence of
-- single-binding lets terminating in `body`.
rebuildLets :: TTermDefinition ([Binding] -> Term -> Term)
rebuildLets = define "rebuildLets" $
  doc "Build a chain of single-binding TermLet wrappers around the given body" $
  lambdas ["bindings", "body"] $
    Lists.foldr
      ("b" ~> "acc" ~> Core.termLet $ Core.let_ (list [var "b"]) (var "acc"))
      (var "body")
      (var "bindings")

-- | Group a list of bindings into SCC components in dependency order,
-- using topological sort over a graph whose edges are the free variables of
-- each binding's term intersected with the set of local binding names.
-- Qualified names like `hydra.foo.bar.f` are additionally matched by their
-- unqualified local part so that references inside a binding term pick up
-- the corresponding local binding even when written without a qualifier.
--
-- Note: the DSL `Sorting.topologicalSortNodes` walks vertices in map-key
-- order (alphabetical), so independent siblings come out in alphabetical
-- rather than source order. This differs from `Data.Graph.stronglyConnComp`
-- (which preserved source order) but is semantically equivalent for Coq's
-- sequential `let ... in`, and the common test suite still compiles 128/132
-- `.v` files — matching the pre-refactor baseline exactly.
processLetSCCs :: TTermDefinition ([Binding] -> [[Binding]])
processLetSCCs = define "processLetSCCs" $
  doc "Sort bindings into SCC groups using free-var analysis and local-name matching" $
  lambda "bindings" $ lets [
    "getName">: lambda "b" $ unwrap _Name @@ (Core.bindingName $ var "b"),
    "names">: Sets.fromList $ Lists.map (var "getName") (var "bindings"),
    "localNames">: Sets.fromList $ Lists.map
      (lambda "b" $ localNameRaw @@ (var "getName" @@ var "b"))
      (var "bindings"),
    "allNames">: Sets.union (var "names") (var "localNames"),
    "depVars">: lambda "b" $ lets [
      "varsName">: Variables.freeVariablesInTerm @@ (Core.bindingTerm $ var "b"),
      "vars">: Sets.fromList $ Lists.map (lambda "n" $ unwrap _Name @@ var "n") (Sets.toList $ var "varsName"),
      "localVars">: Sets.fromList $ Lists.map
        (lambda "v" $ localNameRaw @@ var "v")
        (Sets.toList $ var "vars")] $
      Sets.toList $ Sets.intersection (var "allNames") (Sets.union (var "vars") (var "localVars"))] $
    Sorting.topologicalSortNodes
      @@ var "getName"
      @@ var "depVars"
      @@ var "bindings"

-- | If a term is `hydra_fix (fun innerName => body)`, return `body` with
-- free occurrences of `innerName` substituted by `bName`. Otherwise return
-- the term unchanged. Used while bundling a mutually recursive group so
-- that the outer bundle's fixpoint replaces the per-binding wrappers.
stripHydraFix :: TTermDefinition (Name -> Term -> Term)
stripHydraFix = define "stripHydraFix" $
  doc "Strip an outer hydra_fix lambda wrapper, substituting the inner self-reference for the binding name" $
  lambdas ["bName", "tm"] $ cases _Term (var "tm") (Just $ var "tm") [
    _Term_application>>: "app" ~> lets [
      "fn">: Core.applicationFunction $ var "app",
      "arg">: Core.applicationArgument $ var "app"] $
      cases _Term (var "fn") (Just $ var "tm") [
        _Term_variable>>: "v" ~>
          Logic.ifElse (Equality.equal (unwrap _Name @@ var "v") (string "hydra_fix"))
            (cases _Term (var "arg") (Just $ var "tm") [
              _Term_lambda>>: "lam" ~>
                Variables.substituteVariable
                  @@ (Core.lambdaParameter $ var "lam")
                  @@ var "bName"
                  @@ (Core.lambdaBody $ var "lam")])
            (var "tm")]]

-- | Encode a mutually recursive group of let bindings via hydra_fix bundling.
-- For a group of `n` bindings, emits a single outer let binding
-- `hydra_mutual_bundle_` whose value is a `hydra_fix` over a nested pair of
-- the bindings' (stripped) terms, followed by `n` projection lets that
-- restore access to each individual binding name. See the hand-written
-- original in `heads/haskell/.../Hydra/Coq/Generate.hs` for the emitted
-- Coq shape.
encodeMutualLetGroup :: TTermDefinition ([Binding] -> Term -> Term)
encodeMutualLetGroup = define "encodeMutualLetGroup" $
  doc "Wrap a mutually recursive binding group in a hydra_fix nested-pair bundle with per-name projection lets" $
  lambdas ["grp", "body"] $
  "n" <~ Lists.length (var "grp") $
  "bundleName" <~ (wrap _Name (string "hydra_mutual_bundle_")) $
  "bundleInner" <~ (wrap _Name (string "hydra_mutual_b_")) $
  "innerBundleVar" <~ (Core.termVariable $ var "bundleInner") $
  "outerBundleVar" <~ (Core.termVariable $ var "bundleName") $
  -- appVar fname v = TermApplication (TermVariable fname) v
  "appVar" <~ ("fname" ~> "v" ~> Core.termApplication $ Core.application
    (Core.termVariable $ wrap _Name (var "fname"))
    (var "v")) $
  -- nestedSecond k v: apply pairs.second k times to v
  "nestedSecond" <~ ("k" ~> "v" ~>
    Logic.ifElse (Equality.equal (var "k") (int32 0))
      (var "v")
      (var "appVar" @@ (string "pairs.second") @@
        (var "nestedSecond" @@ (Math.sub (var "k") (int32 1)) @@ var "v"))) $
  -- mkProj bvar i: projection expression for the i-th of n bindings
  "mkProj" <~ ("bvar" ~> "i" ~>
    Logic.ifElse (Equality.equal (var "i") (Math.sub (var "n") (int32 1)))
      (var "nestedSecond" @@ var "i" @@ var "bvar")
      (var "appVar" @@ (string "pairs.first") @@
        (var "nestedSecond" @@ var "i" @@ var "bvar"))) $
  -- Build projection bindings for a given bundle variable
  "mkProjBindings" <~ ("bvar" ~>
    Lists.map ("ib" ~>
      "i" <~ Pairs.first (var "ib") $
      "b" <~ Pairs.second (var "ib") $
      Core.binding
        (Core.bindingName $ var "b")
        (var "mkProj" @@ var "bvar" @@ var "i")
        (Core.bindingTypeScheme $ var "b"))
      (Lists.zip
        (Math.range (int32 0) (Math.sub (var "n") (int32 1)))
        (var "grp"))) $
  "innerProjBindings" <~ (var "mkProjBindings" @@ var "innerBundleVar") $
  "outerProjBindings" <~ (var "mkProjBindings" @@ var "outerBundleVar") $
  -- Strip per-binding hydra_fix wrappers (they're replaced by the bundle's fixpoint)
  "strippedBindings" <~ Lists.map
    ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (stripHydraFix @@ (Core.bindingName $ var "b") @@ (Core.bindingTerm $ var "b"))
      (Core.bindingTypeScheme $ var "b"))
    (var "grp") $
  -- Build a nested pair from the stripped binding terms
  "mkPair" <~ ("ts" ~>
    Maybes.fromMaybe (Core.termVariable $ wrap _Name (string "tt")) (Maybes.map
      (lambda "p" $
        Logic.ifElse (Equality.equal (Lists.length (var "ts")) (int32 1))
          (Pairs.first (var "p"))
          (Core.termPair $ pair
            (Pairs.first (var "p"))
            (var "mkPair" @@ (Pairs.second (var "p")))))
      (Lists.uncons (var "ts")))) $
  "pairExpr" <~ (var "mkPair" @@
    Lists.map ("b" ~> Core.bindingTerm $ var "b") (var "strippedBindings")) $
  "fixBody" <~ (rebuildLets @@ var "innerProjBindings" @@ var "pairExpr") $
  "fixTerm" <~ (Core.termApplication $ Core.application
    (Core.termVariable $ wrap _Name (string "hydra_fix"))
    (Core.termLambda $ Core.lambda (var "bundleInner")
      (Phantoms.nothing :: TTerm (Maybe Type))
      (var "fixBody"))) $
  "bundleBinding" <~ Core.binding (var "bundleName") (var "fixTerm")
    (Phantoms.nothing :: TTerm (Maybe TypeScheme)) $
  Core.termLet $ Core.let_
    (list [var "bundleBinding"])
    (rebuildLets @@ var "outerProjBindings" @@ var "body")

-- | Rebuild a let sequence from a list of SCC groups: each acyclic singleton
-- becomes a plain TermLet, each cyclic group is wrapped via
-- `encodeMutualLetGroup`, and groups are nested in dependency order.
rebuildMutualLets :: TTermDefinition ([[Binding]] -> Term -> Term)
rebuildMutualLets = define "rebuildMutualLets" $
  doc "Rebuild a chain of TermLet/hydra_fix wrappers from SCC-sorted binding groups" $
  lambdas ["groups", "body"] $
    Lists.foldr
      ("grp" ~> "acc" ~> Logic.ifElse
        (Equality.equal (Lists.length (var "grp")) (int32 1))
        (Core.termLet $ Core.let_ (var "grp") (var "acc"))
        (encodeMutualLetGroup @@ var "grp" @@ var "acc"))
      (var "body")
      (var "groups")

-- | Reorder let bindings within a term so that dependencies precede
-- dependents, matching Coq's sequential `let ... in`. At each TermLet node
-- the full chain of nested lets is flattened, grouped into SCCs by
-- free-variable analysis, and rebuilt. Mutually recursive groups get
-- encoded via `encodeMutualLetGroup`.
reorderLetBindings :: TTermDefinition (Term -> Term)
reorderLetBindings = define "reorderLetBindings" $
  doc "Topologically reorder let bindings and pair-encode mutually recursive groups" $
  "term0" ~>
  "f" <~ ("recurse" ~> "tm" ~>
    cases _Term (var "tm") (Just $ var "recurse" @@ var "tm") [
      _Term_let>>: constant $
        "flat" <~ (collectLetBindings @@ var "tm") $
        "allBindings" <~ Pairs.first (var "flat") $
        "innerBody" <~ Pairs.second (var "flat") $
        "groups" <~ (processLetSCCs @@ var "allBindings") $
        "groups2" <~ Lists.map
          ("grp" ~> Lists.map
            ("b" ~> Core.binding
              (Core.bindingName $ var "b")
              (reorderLetBindings @@ (Core.bindingTerm $ var "b"))
              (Core.bindingTypeScheme $ var "b"))
            (var "grp"))
          (var "groups") $
        rebuildMutualLets @@ var "groups2" @@ (reorderLetBindings @@ var "innerBody")]) $
  Rewriting.rewriteTerm @@ var "f" @@ var "term0"

-- | Sort a list of (name, Type) pairs into SCC groups, flagging each group
-- with a Bool indicating whether it is cyclic (mutually recursive, including
-- self-loops). Acyclic singletons produce Coq `Inductive`/`Definition`/
-- `Record` sentences; cyclic groups produce `Inductive ... with ...` blocks.
sortTypeDefsSCC :: TTermDefinition ([(String, Type)] -> [(Bool, [(String, Type)])])
sortTypeDefsSCC = define "sortTypeDefsSCC" $
  doc "Group type definitions into SCC components with a cyclic/acyclic flag" $
  lambda "defs" $ lets [
    "localNames">: Sets.fromList $ Lists.map (lambda "d" $ Pairs.first $ var "d") (var "defs"),
    "depsOf">: lambda "d" $ Sets.toList $ typeRefs @@ var "localNames" @@ (Pairs.second $ var "d"),
    "comps">: Sorting.topologicalSortNodes
      @@ (lambda "d" $ Pairs.first $ var "d")
      @@ (var "depsOf")
      @@ (var "defs")] $
    Lists.map (lambda "grp" $
      Logic.ifElse (Equality.gte (Lists.length $ var "grp") (int32 2))
        (pair (boolean True) (var "grp"))
        -- Singleton: cyclic iff it references itself.
        (Maybes.fromMaybe (pair (boolean False) (var "grp")) (Maybes.map
          (lambda "d" $ lets [
            "name">: Pairs.first $ var "d",
            "deps">: typeRefs @@ var "localNames" @@ (Pairs.second $ var "d")] $
            pair (Sets.member (var "name") (var "deps")) (var "grp"))
          (Lists.maybeHead (var "grp")))))
      (var "comps")

-- | Sort a list of (name, Term) pairs into SCC groups, flagging each group
-- as cyclic or acyclic. Acyclic singletons become `Definition`; cyclic
-- groups get emitted via the `hydra_fix` bundling pipeline.
sortTermDefsSCC :: TTermDefinition ([(String, Term)] -> [(Bool, [(String, Term)])])
sortTermDefsSCC = define "sortTermDefsSCC" $
  doc "Group term definitions into SCC components with a cyclic/acyclic flag" $
  lambda "defs" $ lets [
    "localNames">: Sets.fromList $ Lists.map (lambda "d" $ Pairs.first $ var "d") (var "defs"),
    "depsOf">: lambda "d" $ Sets.toList $ termRefs @@ var "localNames" @@ (Pairs.second $ var "d"),
    "comps">: Sorting.topologicalSortNodes
      @@ (lambda "d" $ Pairs.first $ var "d")
      @@ (var "depsOf")
      @@ (var "defs")] $
    Lists.map (lambda "grp" $
      Logic.ifElse (Equality.gte (Lists.length $ var "grp") (int32 2))
        (pair (boolean True) (var "grp"))
        (Maybes.fromMaybe (pair (boolean False) (var "grp")) (Maybes.map
          (lambda "d" $ lets [
            "name">: Pairs.first $ var "d",
            "deps">: termRefs @@ var "localNames" @@ (Pairs.second $ var "d")] $
            pair (Sets.member (var "name") (var "deps")) (var "grp"))
          (Lists.maybeHead (var "grp")))))
      (var "comps")



