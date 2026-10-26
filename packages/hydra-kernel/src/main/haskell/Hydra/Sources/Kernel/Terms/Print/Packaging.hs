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
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Analysis.ns, PrintCore.ns, Names.ns] L.++ kernelTypesModuleNames),
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
     toDefinition mwisSolve,
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

-- | Exact Maximum-Weight Independent Set solver over a candidate list of module vertices (#804).
-- 'weights' gives each vertex's weight (total reference count), 'adj' gives each vertex's conflict
-- neighbors (modules sharing at least one referenced local name). Returns the (totalWeight,
-- selectedModules) of a maximum-weight independent set. Standard include/exclude branching:
-- OPT(S) = max(OPT(S \ {v}), weight(v) + OPT(S \ ({v} u N(v)))). The first vertex 'v' of the
-- (canonically sorted) list is the branch pivot; the exclude branch recurses on the tail, the
-- include branch recurses on the tail with v's neighbors removed. This is exact (not greedy): it
-- correctly handles the path A-B-C where weight(A)+weight(C) > weight(B) by trying both A+C and B.
--
-- Determinism: the caller passes vertices in canonical (module-name-sorted) order, and ties in
-- objective weight are broken by preferring the INCLUDE branch -- so identical input always yields
-- identical output, which matters for stable regeneration diffs. Zero-weight vertices never improve
-- the objective, so a zero-weight module is only ever selected to stay bare when including it is
-- free (no higher-weight neighbor loses), and never at the expense of a positive-weight module.
--
-- No silent cap: the solver has no vertex limit. It is exponential in the worst case, which is
-- acceptable because real per-module conflict graphs are small and usually sparse/disconnected (the
-- include branch's neighbor-pruning naturally exploits that sparsity). Explicit connected-component
-- decomposition and subset memoization (recommended in #804 for pathological inputs) are deferred
-- performance optimizations, not correctness requirements; if a pathological graph ever makes this
-- too slow, the documented fallback is a greedy heuristic -- never a silent change of result.
mwisSolve :: TypedTermDefinition (M.Map ModuleName Int -> M.Map ModuleName (S.Set ModuleName) -> [ModuleName] -> (Int, [ModuleName]))
mwisSolve = define "mwisSolve" $
  doc "Exact maximum-weight independent set over a candidate list of module vertices (#804)" $
  "weights" ~> "adj" ~> "verts" ~>
  Optionals.match (Lists.uncons (var "verts"))
    (pair (int32 0) (list ([] :: [TypedTerm ModuleName])))
    ("vh" ~>
      "v" <~ Pairs.first (var "vh") $
      "rest" <~ Pairs.second (var "vh") $
      "wv" <~ Optionals.withDefault (int32 0) (Maps.lookup (var "v") (var "weights" :: TypedTerm (M.Map ModuleName Int))) $
      "nbrs" <~ Optionals.withDefault (Sets.empty :: TypedTerm (S.Set ModuleName)) (Maps.lookup (var "v") (var "adj" :: TypedTerm (M.Map ModuleName (S.Set ModuleName)))) $
      -- exclude v: solve the rest as-is
      "excl" <~ (mwisSolve @@ (var "weights" :: TypedTerm (M.Map ModuleName Int)) @@ (var "adj" :: TypedTerm (M.Map ModuleName (S.Set ModuleName))) @@ var "rest") $
      "wExcl" <~ Pairs.first (var "excl") $
      -- include v: solve the rest with v's neighbors removed, then add v and its weight
      "restNoNbr" <~ Lists.filter ("u" ~> Logic.not (Sets.member (var "u") (var "nbrs" :: TypedTerm (S.Set ModuleName)))) (var "rest") $
      "inclSub" <~ (mwisSolve @@ (var "weights" :: TypedTerm (M.Map ModuleName Int)) @@ (var "adj" :: TypedTerm (M.Map ModuleName (S.Set ModuleName))) @@ var "restNoNbr") $
      "wIncl" <~ Math.add (var "wv") (Pairs.first (var "inclSub")) $
      "inclSel" <~ Lists.cons (var "v") (Pairs.second (var "inclSub")) $
      -- tie-break: prefer include when weights are equal (deterministic given canonical vert order)
      Logic.ifElse (Ordering.gte (var "wIncl") (var "wExcl"))
        (pair (var "wIncl") (var "inclSel"))
        (var "excl"))

-- | Compute the printing alias for every module referenced by the given module's definitions
-- (#804 rewrite of the earlier module-name-suffix-collision gate). Per the issue's settled design:
--
-- (1) Collect, per dependency module, the COUNTED set of its local names referenced by the printed
--     module's definitions ('Analysis.definitionDependencyNamesByModule'), plus the printed
--     module's OWN declared local names.
-- (2) A dependency module may be imported BARE (unqualified, no 'as' alias) iff leaving it bare
--     introduces no local-name collision. Two constraints forbid bareness:
--     - inter-import: two dependency modules whose referenced-local-name sets intersect cannot both
--       be bare (the conflict-graph edges);
--     - shadowing: a dependency module whose local-name set intersects the printed module's own
--       declared names can NEVER be bare (a forbidden vertex, dropped before the solver).
-- (3) Among the non-forbidden modules, choose the Maximum-Weight Independent Set to leave bare
--     ('mwisSolve'; weight = total reference count). Selected modules map to "" (printed bare: the
--     import line drops its 'as', and 'Print/Core.printNameAliased' prints their names with no
--     qualifier).
-- (4) Every module NOT selected bare (MWIS losers + forbidden modules) is aliased by the EXISTING,
--     UNCHANGED shortest-unique-suffix fixed point ('growAliasesStep'/'aliasFromSuffix') -- run
--     only over those modules, so the alias machinery stops firing on modules that never needed one.
--
-- The result map's three states feed 'Print/Core.printNameAliased' directly: ""-> bare local name,
-- "<alias>" -> 'alias.Local', absent (never, since every dependency module is a key) -> full dotted.
-- Dependency modules are collected via a pure walk of the definitions' term/type contents (no
-- InferenceContext/Graph needed; the printer always prints a module's own in-memory 'Definition's).
moduleAliases :: TypedTermDefinition (Module -> M.Map ModuleName String)
moduleAliases = define "moduleAliases" $
  doc "Compute the printing alias for every module referenced by the given module's definitions (#804)" $
  "mod" ~>
  "ownNs" <~ Packaging.moduleName (var "mod") $
  "defs" <~ (Packaging.moduleDefinitions $ var "mod") $
  -- Per-module counted referenced local names, excluding self-references to the printed module.
  "namesByModRaw" <~ (Analysis.definitionDependencyNamesByModule @@ var "defs") $
  "namesByMod" <~ Maps.delete (var "ownNs") (var "namesByModRaw" :: TypedTerm (M.Map ModuleName (M.Map String Int))) $
  "nssAsList" <~ Lists.sortBy ("nm" ~> Packaging.unModuleName $ var "nm") (Maps.keys (var "namesByMod" :: TypedTerm (M.Map ModuleName (M.Map String Int)))) $
  -- The printed module's OWN declared local names (for the shadowing constraint).
  "ownLocalNames" <~ Sets.fromList (Lists.map
    ("def" ~> Names.localNameOf @@ (match _Definition (var "def") Nothing [
      _Definition_type>>: "td" ~> Packaging.typeDefinitionName (var "td"),
      _Definition_term>>: "td" ~> Packaging.termDefinitionName (var "td"),
      _Definition_primitive>>: "pd" ~> Packaging.primitiveDefinitionName (var "pd")]))
    (var "defs")) $
  -- The set of referenced local names for a given dependency module.
  "localSetOf" <~ ("nm" ~> Sets.fromList (Maps.keys
    (Optionals.withDefault (Maps.empty :: TypedTerm (M.Map String Int))
      (Maps.lookup (var "nm") (var "namesByMod" :: TypedTerm (M.Map ModuleName (M.Map String Int))))))) $
  -- Vertex weight: total reference count across the module's local names.
  "weightOf" <~ ("nm" ~> Lists.foldl ("a" ~> "b" ~> Math.add (var "a") (var "b")) (int32 0)
    (Maps.elems (Optionals.withDefault (Maps.empty :: TypedTerm (M.Map String Int))
      (Maps.lookup (var "nm") (var "namesByMod" :: TypedTerm (M.Map ModuleName (M.Map String Int))))))) $
  "weights" <~ Maps.fromList (Lists.map ("nm" ~> pair (var "nm") (var "weightOf" @@ var "nm")) (var "nssAsList")) $
  -- Forbidden (never-bare) modules: those whose local names intersect the printed module's own
  -- declared names (local-declaration shadowing). These are excluded from the MWIS candidate set
  -- and always aliased.
  "forbidden" <~ Lists.filter
    ("nm" ~> Logic.not (Sets.isEmpty (Sets.intersection (var "localSetOf" @@ var "nm") (var "ownLocalNames" :: TypedTerm (S.Set String)))))
    (var "nssAsList") $
  "candidates" <~ Lists.filter
    ("nm" ~> Logic.not (Lists.member (var "nm") (var "forbidden")))
    (var "nssAsList") $
  -- Conflict adjacency over the CANDIDATE modules only: an edge between two candidates whose
  -- referenced-local-name sets intersect.
  "adj" <~ Maps.fromList (Lists.map
    ("nm" ~> pair (var "nm") (Sets.fromList (Lists.filter
      ("other" ~> Logic.and
        (Logic.not (Equality.equal (var "other") (var "nm")))
        (Logic.not (Sets.isEmpty (Sets.intersection (var "localSetOf" @@ var "nm") (var "localSetOf" @@ var "other")))))
      (var "candidates"))))
    (var "candidates")) $
  -- Choose the max-weight independent set of candidates to leave bare.
  "bareList" <~ Pairs.second (mwisSolve @@ (var "weights" :: TypedTerm (M.Map ModuleName Int)) @@ (var "adj" :: TypedTerm (M.Map ModuleName (S.Set ModuleName))) @@ var "candidates") $
  "bareSet" <~ Sets.fromList (var "bareList") $
  -- Losers (non-bare candidates) + forbidden modules are aliased via the existing shortest-unique-
  -- suffix fixed point, run over just those modules.
  "aliasedList" <~ Lists.filter ("nm" ~> Logic.not (Sets.member (var "nm") (var "bareSet" :: TypedTerm (S.Set ModuleName)))) (var "nssAsList") $
  "segmentsOf" <~ ("nm" ~> Strings.splitOn (string ".") (Packaging.unModuleName $ var "nm")) $
  "segsMap" <~ Maps.fromList (Lists.map ("nm" ~> pair (var "nm") (var "segmentsOf" @@ var "nm")) (var "aliasedList")) $
  "maxSegs" <~ Lists.foldl
    ("a" ~> "b" ~> Logic.ifElse (Ordering.gt (var "a") (var "b")) (var "a") (var "b"))
    (int32 1)
    (Lists.map ("nm" ~> Lists.length (var "segmentsOf" @@ var "nm")) (var "aliasedList")) $
  "initialState" <~ Maps.fromList (Lists.map ("nm" ~> pair (var "nm") (int32 1)) (var "aliasedList")) $
  "finalState" <~ Lists.foldl (growAliasesStep @@ (var "segsMap" :: TypedTerm (M.Map ModuleName [String]))) (var "initialState") (Lists.replicate (var "maxSegs") unit) $
  -- Assemble the result: bare modules -> "", aliased modules -> their grown dotted alias.
  Maps.fromList (Lists.map
    ("nm" ~> pair (var "nm")
      (Logic.ifElse (Sets.member (var "nm") (var "bareSet" :: TypedTerm (S.Set ModuleName)))
        (string "")
        (aliasFromSuffix @@ (Optionals.withDefault (list ([] :: [TypedTerm String])) (Maps.lookup (var "nm") (var "segsMap" :: TypedTerm (M.Map ModuleName [String]))))
                         @@ (Optionals.withDefault (int32 1) (Maps.lookup (var "nm") (var "finalState" :: TypedTerm (M.Map ModuleName Int)))))))
    (var "nssAsList"))

-- | Show a module dependency as an import declaration, given the printer's computed alias for
-- that dependency (per 'moduleAliases'). The alias map's value for this module decides the form
-- (#804):
--   * ""        -> 'import <module>'            (bare/unqualified -- no local-name collision)
--   * "<alias>" -> 'import <module> as <alias>' (qualified -- a collision forced an alias)
--   * absent    -> 'import <module>'            (bare; a true dependency is always a key, so this
--                                                is a defensive fallback, not an expected state)
-- ModuleDependency carries no alias field, so a module authored with an explicit 'as' does not
-- round-trip its chosen alias through the AST; the printed alias is always the one 'moduleAliases'
-- computes. The 'from <package>' disambiguation form IS written whenever ModuleDependency.package
-- is present (that field exists on the model, so no analogous gap there).
importDecl :: TypedTermDefinition (M.Map ModuleName String -> ModuleDependency -> String)
importDecl = define "importDecl" $
  doc "Show a module dependency as an import declaration, using the given alias map" $
  "aliases" ~> "d" ~>
  "depMod" <~ (Packaging.moduleDependencyModule $ var "d") $
  "modNameStr" <~ (moduleNameString @@ var "depMod") $
  -- An empty-string alias (#804) marks a module imported BARE (unqualified): print a plain
  -- 'import <module>' with no 'as' clause. A non-empty alias prints 'import <module> as <alias>'.
  -- A module absent from the map (should not occur for a true dependency) also prints bare.
  "aliasSuffix" <~ Optionals.match (Maps.lookup (var "depMod") (var "aliases" :: TypedTerm (M.Map ModuleName String)))
    (string "")
    ("alias" ~> Logic.ifElse (Strings.isEmpty $ var "alias")
      (string "")
      (Strings.concat $ list [string " as ", var "alias"])) $
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
-- The import-alias map (#803) is threaded into the body printer so nominal references in the
-- type are shortened via the module's own 'import ... as <alias>' lines.
typeDefinition :: TypedTermDefinition (M.Map ModuleName String -> TypeDefinition -> String)
typeDefinition = define "typeDefinition" $
  doc "Show a type definition as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "td" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.typeDefinitionName $ var "td")) $
  "typeStr" <~ (PrintCore.typeSchemeAliased @@ var "aliases" @@ (Packaging.typeDefinitionBody $ var "td")) $
  withDefinitionDoc @@ (Packaging.typeDefinitionMetadata $ var "td") @@
    (Strings.concat $ list [string "type ", var "localName", string " = ", var "typeStr"])

-- | Show a term definition: 'term name [: <typeScheme>] = <term>', with its doc-comment block
-- (if any) prepended. Signature is printed only when present -- conventionally omitted per #710
-- design (left for inference).
termDefinition :: TypedTermDefinition (M.Map ModuleName String -> TermDefinition -> String)
termDefinition = define "termDefinition" $
  doc "Show a term definition as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "td" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.termDefinitionName $ var "td")) $
  -- sigStr carries its own leading/trailing spacing (or is empty) so no separate literal space
  -- is needed around it below.
  "sigStr" <~ Optionals.match (Packaging.termDefinitionSignature $ var "td")
    (string " ")
    ("sig" ~> Strings.concat $ list [string " : ", PrintCore.termSignatureAliased @@ var "aliases" @@ var "sig", string " "]) $
  "bodyStr" <~ (PrintCore.termAliased @@ var "aliases" @@ (Packaging.termDefinitionBody $ var "td")) $
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
primitiveDefinitionDoc :: TypedTermDefinition (M.Map ModuleName String -> PrimitiveDefinition -> String)
primitiveDefinitionDoc = define "primitiveDefinition" $
  doc "Show a primitive definition as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "pd" ~>
  "localName" <~ (Names.localNameOf @@ (Packaging.primitiveDefinitionName $ var "pd")) $
  "impureStr" <~ Logic.ifElse (Packaging.primitiveDefinitionIsPure $ var "pd")
    (string "")
    (string "impure ") $
  "partialStr" <~ Logic.ifElse (Packaging.primitiveDefinitionIsTotal $ var "pd")
    (string "")
    (string "partial ") $
  "sigStr" <~ (PrintCore.termSignatureAliased @@ var "aliases" @@ (Packaging.primitiveDefinitionSignature $ var "pd")) $
  "implStr" <~ Optionals.match (Packaging.primitiveDefinitionDefaultImplementation $ var "pd")
    (string "")
    ("impl" ~> Strings.concat $ list [string " = ", PrintCore.termAliased @@ var "aliases" @@ var "impl"]) $
  withDefinitionDoc @@ (Packaging.primitiveDefinitionMetadata $ var "pd") @@
    (Strings.concat $ list [
      var "impureStr", var "partialStr",
      string "primitive ", var "localName", string " : ", var "sigStr", var "implStr"])

-- | Show a definition (type, term, or primitive) by dispatching to the matching printer, threading
-- the import-alias map (#803) into the body/signature printers.
definition :: TypedTermDefinition (M.Map ModuleName String -> Definition -> String)
definition = define "definition" $
  doc "Show a definition as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "def" ~> match _Definition (var "def") Nothing [
    _Definition_primitive>>: "pd" ~> primitiveDefinitionDoc @@ var "aliases" @@ var "pd",
    _Definition_term>>: "td" ~> termDefinition @@ var "aliases" @@ var "td",
    _Definition_type>>: "tyd" ~> typeDefinition @@ var "aliases" @@ var "tyd"]

-- | Show an entire module: header, imports, then definitions in the order given (#710: the
-- grammar imposes no ordering requirement; this printer preserves 'Module.definitions' list
-- order as given rather than re-sorting).
--
-- Import-line aliasing: computed per syntax.md §6.4's settled algorithm (see 'moduleAliases'),
-- so every printed import line carries an 'as <alias>' clause chosen by the asymmetric
-- shortest-wins collision rule. The SAME 'moduleAliases' map is now threaded into definition
-- BODIES too (#803): 'definition' -> 'typeDefinition'/'termDefinition'/'primitiveDefinitionDoc'
-- -> 'PrintCore.{typeSchemeAliased,termAliased,termSignatureAliased}', so every nominal reference
-- in a term/type body is shortened via that same alias map. This makes the printed 'import ... as
-- <alias>' lines load-bearing (the body uses the alias) rather than advertising a shortening the
-- body never took. The alias forms round-trip: 'Parse/Packaging.hs's 'expandAliasesInDefinition'
-- re-expands each aliased first segment back to its full module name using the same import table.
-- Callers with no module context (tests, the lexicon generator, the JSON coder, standalone spec
-- examples) keep the fully-dotted output because they call 'PrintCore.{term,type_,typeScheme,
-- termSignature}', whose empty-alias-map delegation reproduces the pre-#803 behaviour exactly.
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
  "defStrs" <~ Lists.map (definition @@ (var "aliases" :: TypedTerm (M.Map ModuleName String))) (Packaging.moduleDefinitions $ var "mod") $
  Strings.join (string "\n\n") (Lists.cons (var "headerStr")
    (Lists.cons (Strings.join (string "\n") (var "importStrs"))
      (var "defStrs")))
