{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Print.Packaging where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType)
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Ordering    as Ordering
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (
  binding, field, fields, fieldType, floatType, floatValue, injection, integerType,
  integerValue, lambda, literal, literalType, project, term, type_, typeScheme)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Analysis as Analysis
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping


-- NOTE: this module is a work-in-progress skeleton for #710. All syntax-design questions were
-- settled 2026-10-11 (see claude-hydra-messages/outbox/draft-syntax-extension-497-and-710.md,
-- now a pure historical record with zero open items) and doc-comment printing (EntityMetadata ->
-- '##' comments) is fully implemented below. The one remaining TODO is per-dependency 'as'-alias
-- printing (see importDecl's own comment) -- not a design gap, but blocked on PrintCore itself
-- gaining a minimal-name-choosing environment parameter (not yet implemented upstream in #497).
-- Builds against feature_497_print_parse_syntax's current (W4, post-printer-rewrite) Print/Core.hs
-- (now landed on origin/main, see branch plan 2026-10-09 entry); NOT YET build-verified in this
-- worktree due to stack-slot queue contention (see branch plan 2026-10-11 entries).

ns :: ModuleName
ns = ModuleName "hydra.core.print.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Analysis.ns, PrintCore.ns, Names.ns, Scoping.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.packaging types")}
  where
   definitions = [
     toDefinition aliasFromSuffix,
     toDefinition definition,
     toDefinition entityMetadataDoc,
     toDefinition entityReferenceDoc,
     toDefinition growAliasesStep,
     toDefinition importDecl,
     toDefinition moduleAliases,
     toDefinition module',
     toDefinition moduleNameString,
     toDefinition primitiveDefinitionDoc,
     toDefinition termDefinition,
     toDefinition typeDefinition,
     toDefinition withDefinitionDoc]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Show a module name as a dotted string, e.g. "hydra.example.geometry".
moduleNameString :: TypedTermDefinition (ModuleName -> String)
moduleNameString = define "moduleNameString" $
  doc "Show a module name as a string" $
  "n" ~> Packaging.unModuleName (var "n")

-- | Show an EntityMetadata as a block of '##' doc-comment lines, per the §6.6 algorithm settled
-- 2026-10-11 (draft-syntax-md-section6-modules.md). Printing is the easy direction of that
-- algorithm -- no fixpoint/section-splitting is needed, since printing starts from the
-- already-split description/comments/seeAlso/lifecycle/provisions fields and just needs to emit
-- them in a fixed, parseable order: description, then each comments entry (each its own
-- "section", i.e. separated by a blank '##' line from its neighbors so the parser's
-- double-newline-separator rule recovers them as distinct comments entries), then a blank-line-
-- separated block of '@key' lines (seeAlso*, lifecycle, provisions*, in that order -- order among
-- @key lines themselves is not semantically significant per the parsing algorithm, but a FIXED
-- print order keeps output deterministic/diffable).
--
-- Returns the empty string for a totally-empty EntityMetadata (no description, no comments, no
-- seeAlso, no lifecycle, no provisions) -- callers must handle whether/how to separate this
-- (empty) block from the following declaration themselves; see 'definitionDoc' usage in
-- typeDefinition/termDefinition/primitiveDefinition below.
entityMetadataDoc :: TypedTermDefinition (EntityMetadata -> String)
entityMetadataDoc = define "entityMetadataDoc" $
  doc "Show an EntityMetadata as a block of '##' doc-comment lines" $
  "em" ~>
  "descLine" <~ Optionals.match (Packaging.entityMetadataDescription $ var "em")
    (list ([] :: [TypedTerm String]))
    ("d" ~> list [Strings.concat2 (string "## ") (var "d")]) $
  "commentBlocks" <~ Lists.map
    ("c" ~> Strings.concat2 (string "## ") (var "c"))
    (Packaging.entityMetadataComments $ var "em") $
  "seeAlsoLines" <~ Lists.map
    ("ref" ~> Strings.concat $ list [string "## @seeAlso ", entityReferenceDoc @@ var "ref"])
    (Packaging.entityMetadataSeeAlso $ var "em") $
  "lifecycleLines" <~ Optionals.match (Packaging.entityMetadataLifecycle $ var "em")
    (list ([] :: [TypedTerm String]))
    ("lc" ~>
      "avail" <~ Optionals.match (Packaging.lifecycleInfoAvailableSince $ var "lc")
        (list ([] :: [TypedTerm String]))
        ("v" ~> list [Strings.concat2 (string "## @availableSince ") (Packaging.unVersion $ var "v")]) $
      "depr" <~ Optionals.match (Packaging.lifecycleInfoDeprecatedSince $ var "lc")
        (list ([] :: [TypedTerm String]))
        ("v" ~> list [Strings.concat2 (string "## @deprecatedSince ") (Packaging.unVersion $ var "v")]) $
      Lists.concat2 (var "avail") (var "depr")) $
  "provisionLines" <~ Lists.map
    ("p" ~> lets [
      "localName">: Names.localNameOf @@ (Packaging.provisionName $ var "p"),
      "tag">: match _ProvisionKind (Packaging.provisionKind $ var "p") Nothing [
        _ProvisionKind_requirement>>: constant $ string "@requirement",
        _ProvisionKind_recommendation>>: constant $ string "@recommendation"]] $
      Strings.concat $ list [string "## ", var "tag", string " ", var "localName", string " ", Packaging.provisionStatement $ var "p"])
    (Packaging.entityMetadataProvisions $ var "em") $
  "keyLines" <~ Lists.concat (list [var "seeAlsoLines", var "lifecycleLines", var "provisionLines"]) $
  "sections" <~ Lists.filter ("s" ~> Logic.not (Lists.isEmpty (var "s"))) (list [
    var "descLine",
    var "commentBlocks",
    var "keyLines"]) $
  Strings.join (string "\n##\n") (Lists.map ("s" ~> Strings.join (string "\n") (var "s")) (var "sections"))

-- | Show an EntityReference per #710's §6.6 tag-keyword convention (settled 2026-10-11, reusing
-- Parse/Docs.hs's existing {@tag rhs} doc-string annotation tags). Used only as @seeAlso's
-- payload printer here; termExpr/typeExpr already hold pre-rendered Hydra textual syntax
-- (EntityReference models them as String, not Term/Type), so they're printed as-is.
entityReferenceDoc :: TypedTermDefinition (EntityReference -> String)
entityReferenceDoc = define "entityReferenceDoc" $
  doc "Show an EntityReference using the type/term/primitive/module/package/term-expr/type-expr tags" $
  "ref" ~> match _EntityReference (var "ref") Nothing [
    _EntityReference_definition>>: "defRef" ~> match _DefinitionReference (var "defRef") Nothing [
      _DefinitionReference_type>>: "n" ~> Strings.concat2 (string "type ") (Core.unName $ var "n"),
      _DefinitionReference_term>>: "n" ~> Strings.concat2 (string "term ") (Core.unName $ var "n"),
      _DefinitionReference_primitive>>: "n" ~> Strings.concat2 (string "primitive ") (Core.unName $ var "n")],
    _EntityReference_module>>: "mn" ~> Strings.concat2 (string "module ") (Packaging.unModuleName $ var "mn"),
    _EntityReference_package>>: "pn" ~> Strings.concat2 (string "package ") (Packaging.unPackageName $ var "pn"),
    _EntityReference_termExpr>>: "t" ~> Strings.concat2 (string "term-expr ") (var "t"),
    _EntityReference_typeExpr>>: "t" ~> Strings.concat2 (string "type-expr ") (var "t")]

-- | Build a candidate alias from the last `n` dotted segments of a module's full name, e.g.
-- ["hydra","core","model"] with n=1 gives "model", with n=2 gives "core.model". Multi-segment
-- dotted aliases are valid textual-syntax aliases (syntax.md §6.3), so unlike the Haskell coder's
-- own 'namespacesForModule' (which PascalCase-joins the suffix into one Haskell identifier), this
-- keeps the suffix dotted as-is -- no host-specific join step needed.
aliasFromSuffix :: TypedTermDefinition ([String] -> Int -> String)
aliasFromSuffix = define "aliasFromSuffix" $
  doc "Build a candidate alias from the last n dotted segments of a module's full name" $
  "segs" ~> "n" ~>
  "dropCount" <~ Math.sub (Lists.length $ var "segs") (var "n") $
  Strings.join (string ".") (Lists.drop (var "dropCount") (var "segs"))

-- | One growth round of the alias-collision fixed point (syntax.md §6.4's "shortest module name
-- wins the shortest alias" rule): within each group of dependency modules whose current candidate
-- alias collides, every member strictly longer than the group's shortest member grows by one
-- trailing segment; the shortest member (if it alone is shortest) keeps its current alias. Ported
-- from Hydra.Sources.Haskell.Utils.namespacesForModule's 'growStep', dropping only the
-- PascalCase-join step that Haskell-specific encoding needs on top.
growAliasesStep :: TypedTermDefinition (M.Map ModuleName [String] -> M.Map ModuleName Int -> () -> M.Map ModuleName Int)
growAliasesStep = define "growAliasesStep" $
  doc "One growth round of the alias-collision fixed point: colliding non-shortest members grow" $
  "segsMap" ~> "state" ~> "_ign" ~>
  "nss" <~ Maps.keys (var "segsMap" :: TypedTerm (M.Map ModuleName [String])) $
  "segsFor" <~ ("nm" ~> Optionals.withDefault (list ([] :: [TypedTerm String])) (Maps.lookup (var "nm") (var "segsMap" :: TypedTerm (M.Map ModuleName [String])))) $
  "takenFor" <~ ("nm" ~> Optionals.withDefault (int32 1) (Maps.lookup (var "nm") (var "state" :: TypedTerm (M.Map ModuleName Int)))) $
  "entries" <~ Lists.map
    ("nm" ~> lets [
      "segs">: var "segsFor" @@ var "nm",
      "n">: var "takenFor" @@ var "nm",
      "segCount">: Lists.length $ var "segs",
      "aliasStr">: aliasFromSuffix @@ var "segs" @@ var "n"] $
      pair (var "nm") (pair (var "n") (pair (var "segCount") (var "aliasStr"))))
    (var "nss") $
  "aliasKey" <~ ("e" ~> Pairs.second $ Pairs.second $ Pairs.second $ var "e") $
  "segCountOf" <~ ("e" ~> Pairs.first $ Pairs.second $ Pairs.second $ var "e") $
  "aliasCounts" <~ Lists.foldl
    ("m" ~> "e" ~> Maps.insert (var "aliasKey" @@ var "e")
      (Math.add (int32 1) (Optionals.withDefault (int32 0) (Maps.lookup (var "aliasKey" @@ var "e") (var "m" :: TypedTerm (M.Map String Int)))))
      (var "m" :: TypedTerm (M.Map String Int)))
    (Maps.empty :: TypedTerm (M.Map String Int))
    (var "entries") $
  "aliasMinSegs" <~ Lists.foldl
    ("m" ~> "e" ~>
      "k" <~ var "aliasKey" @@ var "e" $
      "segCount" <~ var "segCountOf" @@ var "e" $
      Maps.insert (var "k")
        (Optionals.match (Maps.lookup (var "k") (var "m" :: TypedTerm (M.Map String Int)))
          (var "segCount")
          ("prev" ~> Logic.ifElse (Ordering.lt (var "segCount") (var "prev")) (var "segCount") (var "prev")))
        (var "m" :: TypedTerm (M.Map String Int)))
    (Maps.empty :: TypedTerm (M.Map String Int))
    (var "entries") $
  "aliasMinSegsCount" <~ Lists.foldl
    ("m" ~> "e" ~>
      "k" <~ var "aliasKey" @@ var "e" $
      "segCount" <~ var "segCountOf" @@ var "e" $
      "minSegs" <~ Optionals.withDefault (var "segCount") (Maps.lookup (var "k") (var "aliasMinSegs" :: TypedTerm (M.Map String Int))) $
      Logic.ifElse (Equality.equal (var "segCount") (var "minSegs"))
        (Maps.insert (var "k")
          (Math.add (int32 1) (Optionals.withDefault (int32 0) (Maps.lookup (var "k") (var "m" :: TypedTerm (M.Map String Int)))))
          (var "m" :: TypedTerm (M.Map String Int)))
        (var "m" :: TypedTerm (M.Map String Int)))
    (Maps.empty :: TypedTerm (M.Map String Int))
    (var "entries") $
  Maps.fromList (Lists.map
    ("e" ~>
      "nm" <~ Pairs.first (var "e") $
      "n" <~ Pairs.first (Pairs.second (var "e")) $
      "segCount" <~ var "segCountOf" @@ var "e" $
      "aliasStr" <~ var "aliasKey" @@ var "e" $
      "count" <~ Optionals.withDefault (int32 0) (Maps.lookup (var "aliasStr") (var "aliasCounts" :: TypedTerm (M.Map String Int))) $
      "minSegs" <~ Optionals.withDefault (var "segCount") (Maps.lookup (var "aliasStr") (var "aliasMinSegs" :: TypedTerm (M.Map String Int))) $
      "minSegsCount" <~ Optionals.withDefault (int32 0) (Maps.lookup (var "aliasStr") (var "aliasMinSegsCount" :: TypedTerm (M.Map String Int))) $
      "canGrow" <~ Logic.and
        (Ordering.gt (var "count") (int32 1))
        (Logic.and
          (Ordering.gt (var "segCount") (var "n"))
          (Logic.or
            (Ordering.gt (var "segCount") (var "minSegs"))
            (Ordering.gt (var "minSegsCount") (int32 1)))) $
      pair (var "nm") (Logic.ifElse (var "canGrow") (Math.add (var "n") (int32 1)) (var "n")))
    (var "entries"))

-- | Compute the printing alias for every module referenced by the given module's definitions,
-- per syntax.md §6.4's settled algorithm: run the alias-collision fixed point (bounded by the
-- longest dotted name among the dependencies, which is always enough rounds to reach a
-- collision-free assignment), then render each winning (module, segmentsTaken) pair as a dotted
-- alias string. Dependency modules are collected via 'Analysis.definitionDependencyModuleNames'
-- (a pure walk of the definitions' term/type contents -- no InferenceContext/Graph needed, unlike
-- the Haskell coder's 'namespacesForModule', which additionally needs those to also fold in
-- modules referenced only through reified/encoded data; #710's printer does not have that
-- concern, since it is always printing a module's own in-memory 'Definition's directly).
moduleAliases :: TypedTermDefinition (Module -> M.Map ModuleName String)
moduleAliases = define "moduleAliases" $
  doc "Compute the printing alias for every module referenced by the given module's definitions" $
  "mod" ~>
  "ownNs" <~ Packaging.moduleName (var "mod") $
  "nss" <~ Sets.delete (var "ownNs") (Analysis.definitionDependencyModuleNames @@ (Packaging.moduleDefinitions $ var "mod")) $
  "nssAsList" <~ Sets.toList (var "nss" :: TypedTerm (S.Set ModuleName)) $
  "segmentsOf" <~ ("nm" ~> Strings.splitOn (string ".") (Packaging.unModuleName $ var "nm")) $
  "segsMap" <~ Maps.fromList (Lists.map ("nm" ~> pair (var "nm") (var "segmentsOf" @@ var "nm")) (var "nssAsList")) $
  "maxSegs" <~ Lists.foldl
    ("a" ~> "b" ~> Logic.ifElse (Ordering.gt (var "a") (var "b")) (var "a") (var "b"))
    (int32 1)
    (Lists.map ("nm" ~> Lists.length (var "segmentsOf" @@ var "nm")) (var "nssAsList")) $
  "initialState" <~ Maps.fromList (Lists.map ("nm" ~> pair (var "nm") (int32 1)) (var "nssAsList")) $
  "finalState" <~ Lists.foldl (growAliasesStep @@ (var "segsMap" :: TypedTerm (M.Map ModuleName [String]))) (var "initialState") (Lists.replicate (var "maxSegs") unit) $
  Maps.fromList (Lists.map
    ("nm" ~> pair (var "nm")
      (aliasFromSuffix @@ (Optionals.withDefault (list ([] :: [TypedTerm String])) (Maps.lookup (var "nm") (var "segsMap" :: TypedTerm (M.Map ModuleName [String]))))
                       @@ (Optionals.withDefault (int32 1) (Maps.lookup (var "nm") (var "finalState" :: TypedTerm (M.Map ModuleName Int))))))
    (var "nssAsList"))

-- | Show a module dependency as an import declaration, given the printer's computed alias for
-- that dependency (per 'moduleAliases'/syntax.md §6.4). Always prints 'as <alias>' -- the
-- "always import every referenced module, using the computed alias" default (syntax.md §6.4,
-- confirmed safe/correct, the import-or-not policy question left open and non-blocking). Per
-- #710 design (settled 2026-10-08):
-- ModuleDependency carries no alias field, so there is currently NO WAY to print one even if a
-- module was originally authored with 'as'. This is a tracked ROUND-TRIP GAP, not just a missing
-- feature: open question #0 in draft-syntax-extension-497-and-710.md (found 2026-10-08) flags
-- that parse-then-print of 'import X as Y' silently drops the alias today, which is NOT
-- surface-trivia loss (unlike whitespace/parenthesization) -- it's authored information. Not
-- fixed here; needs Josh's call on whether ModuleDependency gains an alias field or aliasing
-- stays print/parse-local with no AST representation (see that question for both options). The
-- 'from <package>' disambiguation form IS written whenever ModuleDependency.package is present
-- (that field exists on the model, so no analogous gap there).
importDecl :: TypedTermDefinition (M.Map ModuleName String -> ModuleDependency -> String)
importDecl = define "importDecl" $
  doc "Show a module dependency as an import declaration, using the given alias map" $
  "aliases" ~> "d" ~>
  "depMod" <~ (Packaging.moduleDependencyModule $ var "d") $
  "modNameStr" <~ (moduleNameString @@ var "depMod") $
  "aliasSuffix" <~ Optionals.match (Maps.lookup (var "depMod") (var "aliases" :: TypedTerm (M.Map ModuleName String)))
    (string "")
    ("alias" ~> Strings.concat $ list [string " as ", var "alias"]) $
  "pkgSuffix" <~ Optionals.match (Packaging.moduleDependencyPackage $ var "d")
    (string "")
    ("pkg" ~> Strings.concat $ list [string " from ", Packaging.unPackageName (var "pkg")]) $
  Strings.concat $ list [string "import ", var "modNameStr", var "aliasSuffix", var "pkgSuffix"]

-- | Prepend a definition's doc-comment block (if its metadata is non-empty) to the given
-- already-printed declaration string, separated by a newline. Shared by all three definition
-- printers below.
withDefinitionDoc :: TypedTermDefinition (Maybe EntityMetadata -> String -> String)
withDefinitionDoc = define "withDefinitionDoc" $
  doc "Prepend a definition's '##' doc-comment block to its printed declaration, if present" $
  "mmeta" ~> "declStr" ~>
  Optionals.match (var "mmeta")
    (var "declStr")
    ("meta" ~>
      "docBlock" <~ (entityMetadataDoc @@ var "meta") $
      Logic.ifElse (Strings.isEmpty $ var "docBlock")
        (var "declStr")
        (Strings.concat $ list [var "docBlock", string "\n", var "declStr"]))

-- | Show a type definition: 'type Name = <type>', with its doc-comment block (if any) prepended.
typeDefinition :: TypedTermDefinition (TypeDefinition -> String)
typeDefinition = define "typeDefinition" $
  doc "Show a type definition as a string" $
  "td" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.typeDefinitionName $ var "td")) $
  "typeStr" <~ (PrintCore.typeScheme @@ (Packaging.typeDefinitionBody $ var "td")) $
  withDefinitionDoc @@ (Packaging.typeDefinitionMetadata $ var "td") @@
    (Strings.concat $ list [string "type ", var "localName", string " = ", var "typeStr"])

-- | Show a term definition: 'term name [: <typeScheme>] = <term>', with its doc-comment block
-- (if any) prepended. Signature is printed only when present -- conventionally omitted per #710
-- design (left for inference).
termDefinition :: TypedTermDefinition (TermDefinition -> String)
termDefinition = define "termDefinition" $
  doc "Show a term definition as a string" $
  "td" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.termDefinitionName $ var "td")) $
  -- sigStr carries its own leading/trailing spacing (or is empty) so no separate literal space
  -- is needed around it below.
  "sigStr" <~ Optionals.match (Packaging.termDefinitionSignature $ var "td")
    (string " ")
    ("sig" ~> Strings.concat $ list [string " : ", PrintCore.typeScheme @@ (Scoping.termSignatureToTypeScheme @@ var "sig"), string " "]) $
  "bodyStr" <~ (PrintCore.term @@ (Packaging.termDefinitionBody $ var "td")) $
  withDefinitionDoc @@ (Packaging.termDefinitionMetadata $ var "td") @@
    (Strings.concat $ list [string "term ", var "localName", var "sigStr", string "= ", var "bodyStr"])

-- | Show a primitive definition: '[impure] [partial] primitive name : <typeScheme> [= <term>]',
-- with its doc-comment block (if any) prepended. isPure/isTotal surface as prefix keywords
-- (impure before partial), omitted when both true -- the common case, matching the Haskell
-- DSL's own default (see Phantoms.hs's primitiveInModule, which hardcodes True True).
-- defaultImplementation reuses plain term syntax after '=', exactly like a term definition's
-- body.
-- NOTE: Haskell binding named primitiveDefinitionDoc, not primitiveDefinition, to avoid an
-- ambiguous-occurrence clash with Primitive.primitiveDefinition (a field of the unrelated
-- hydra.core.graph.Primitive record, also in scope via the broad Hydra.Kernel import). The DSL
-- term name registered below ("primitiveDefinition") is unaffected -- only the Haskell-level
-- identifier changed.
primitiveDefinitionDoc :: TypedTermDefinition (PrimitiveDefinition -> String)
primitiveDefinitionDoc = define "primitiveDefinition" $
  doc "Show a primitive definition as a string" $
  "pd" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.primitiveDefinitionName $ var "pd")) $
  "impureStr" <~ Logic.ifElse (Packaging.primitiveDefinitionIsPure $ var "pd")
    (string "")
    (string "impure ") $
  "partialStr" <~ Logic.ifElse (Packaging.primitiveDefinitionIsTotal $ var "pd")
    (string "")
    (string "partial ") $
  "sigStr" <~ (PrintCore.typeScheme @@ (Scoping.termSignatureToTypeScheme @@ (Packaging.primitiveDefinitionSignature $ var "pd"))) $
  "implStr" <~ Optionals.match (Packaging.primitiveDefinitionDefaultImplementation $ var "pd")
    (string "")
    ("impl" ~> Strings.concat $ list [string " = ", PrintCore.term @@ var "impl"]) $
  withDefinitionDoc @@ (Packaging.primitiveDefinitionMetadata $ var "pd") @@
    (Strings.concat $ list [
      var "impureStr", var "partialStr",
      string "primitive ", var "localName", string " : ", var "sigStr", var "implStr"])

-- | Show a definition (type, term, or primitive) by dispatching to the matching printer.
definition :: TypedTermDefinition (Definition -> String)
definition = define "definition" $
  doc "Show a definition as a string" $
  "def" ~> match _Definition (var "def") Nothing [
    _Definition_primitive>>: primitiveDefinitionDoc,
    _Definition_term>>: termDefinition,
    _Definition_type>>: typeDefinition]

-- | Show an entire module: header, imports, then definitions in the order given (#710: the
-- grammar imposes no ordering requirement; this printer preserves 'Module.definitions' list
-- order as given rather than re-sorting).
--
-- Import-line aliasing: computed per syntax.md §6.4's settled algorithm (see 'moduleAliases'),
-- so every printed import line carries an 'as <alias>' clause chosen by the asymmetric
-- shortest-wins collision rule. SCOPED: aliasing is applied only to the import lines themselves;
-- definition BODIES (via 'definition' -> 'typeDefinition'/'termDefinition'/
-- 'primitiveDefinitionDoc' -> 'PrintCore.term'/'typeScheme') still print every name fully-dotted.
-- Threading the alias map into body printing too would require either (a) modifying
-- 'PrintCore.term'/'type_'/'typeScheme' to accept a name-printing environment (invasive --
-- 'PrintCore' is used unchanged by the JSON coder, lexicon generator, and every other fully-dotted
-- consumer; the 497 W4 printer is stable and shouldn't change shape for this), or (b) a
-- full from-scratch duplicate of 'PrintCore.termAtPrec'/'typeAtPrec' local to this module (a
-- ~500-line precedence-aware printer, high risk of drifting from PrintCore's own correctness).
-- Neither is justified by the round-trip contract (§1): fully-dotted body output is always valid,
-- never incorrect, just more verbose than necessary -- a readability optimization, not a
-- correctness requirement. So this is a conscious, documented scope boundary, not an oversight:
-- import lines get real aliases (the genuinely settled, implementable part of §6.4); body
-- printing stays fully-dotted until a future change either extends PrintCore itself or a
-- lighter-weight env-threading mechanism is designed for it.
--
-- Module-level doc-comment printing: per §6.2's grammar (module ::= moduleDoc? moduleHeader
-- importDecl* definition*), the doc block precedes the header.
module' :: TypedTermDefinition (Module -> String)
module' = define "moduleDefinition" $
  doc "Show a module as a string" $
  "mod" ~>
  "headerStr" <~ (withDefinitionDoc @@ (Packaging.moduleMetadata $ var "mod") @@
    (Strings.concat (list [string "module ", moduleNameString @@ (Packaging.moduleName $ var "mod")]))) $
  "aliases" <~ (moduleAliases @@ var "mod") $
  "importStrs" <~ Lists.map (importDecl @@ (var "aliases" :: TypedTerm (M.Map ModuleName String))) (Packaging.moduleDependencies $ var "mod") $
  "defStrs" <~ Lists.map (asTerm definition) (Packaging.moduleDefinitions $ var "mod") $
  Strings.join (string "\n\n") (Lists.cons (var "headerStr")
    (Lists.cons (Strings.join (string "\n") (var "importStrs"))
      (var "defStrs")))
