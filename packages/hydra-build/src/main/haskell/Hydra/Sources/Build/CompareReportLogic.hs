module Hydra.Sources.Build.CompareReportLogic where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Build.Walk as Walk


-- | Pure snapshot-compare decision helpers (#416 P4). The three native generator
-- drivers (generate-hydra-{java,python,scala}-from-*.sh) each hand-rolled an
-- inline-Python heredoc to compare freshly-generated JSON against the committed
-- canonical dist/json, tallying byte-identical vs. differing modules and shelling
-- out to @diff@ for a differing-line count. This module promotes the pure decision;
-- the effect (listDirectory + readFile) stays in a host-native executor that reads
-- both sides and calls these helpers, following the same pure-decision/native-
-- executor split as 'Hydra.Sources.Build.Walk' and the assembly plan.
--
-- Following the established hydra-build pattern (assemblyplan), the pure core works
-- in TUPLES rather than constructing the package-local typed record values
-- (@CompareReport@\/@ModuleCompare@\/@CompareStatus@ from
-- 'Hydra.Sources.Build.CompareReport'): a compared module is a
-- @(moduleName, statusTag, diffLines)@ triple, where @statusTag@ is one of the
-- string tags @"byteEq"@\/@"differ"@\/@"missing"@ (mirroring the @CompareStatus@
-- union variants), and the whole report is a
-- @([(moduleName, statusTag, diffLines)], byteEqCount, total)@ triple. The typed
-- record model is the schema the native executor maps onto; the byte sizes
-- (@ourSize@\/@canonSize@) are a trivial @length@ the executor derives from the
-- content it already read, so they are not carried in the pure tuple. This keeps
-- the module free of any dependency on generated @_CompareStatus_*@ field\/variant
-- constants (the generator-imports-generated concern the tuple pattern avoids).
ns :: ModuleName
ns = ModuleName "hydra.build.comparereportlogic"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Walk.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Pure snapshot-compare decision helpers: per-module status/diff-count and the whole-report tally, over content both sides already read")}
  where
   definitions = [
     toDefinition buildReport,
     toDefinition compareModule,
     toDefinition diffLineCount,
     toDefinition moduleNamesFromEntries,
     toDefinition renderReport,
     toDefinition renderRow,
     toDefinition reportPassed,
     toDefinition stripJsonSuffix]

-- | The whole report over a list of enumerated module triples
-- @(moduleName, ourContent, canonContent)@ (each content @Nothing@ when that side
-- is absent). Folds 'compareModule' over the inputs to the ordered row triples,
-- and tallies how many were byte-identical (@"byteEq"@) plus the total. Result is
-- @(rows, byteEqCount, total)@.
buildReport :: TypedTermDefinition ([(String, Maybe String, Maybe String)] -> ([(String, String, Int)], Int, Int))
buildReport = define "buildReport" $
  doc "The whole compare-report (rows, byteEqCount, total) over enumerated module triples" $
  "inputs" ~>
  "rows" <~ Lists.map ("t" ~>
      compareModule
        @@ (Pairs.first (var "t"))
        @@ (Pairs.first (Pairs.second (var "t")))
        @@ (Pairs.second (Pairs.second (var "t")))) (var "inputs") $
  "byteEqCount" <~ Lists.length
      (Lists.filter ("r" ~> Equality.equal (Pairs.first (Pairs.second (var "r"))) (string "byteEq"))
        (var "rows")) $
  triple (var "rows") (var "byteEqCount") (Lists.length (var "rows"))

-- | Compare one module given both sides already read. @Nothing@ on our side means
-- the module is @"missing"@ (the canonical side has it, ours does not); if both
-- sides are present they are @"byteEq"@ when the contents are equal, else
-- @"differ"@ with a 'diffLineCount'. Returns a @(moduleName, statusTag, diffLines)@
-- triple; @diffLines@ is 0 for @"byteEq"@ and @"missing"@.
compareModule :: TypedTermDefinition (String -> Maybe String -> Maybe String -> (String, String, Int))
compareModule = define "compareModule" $
  doc "Compare one module (both sides pre-read) to a (name, statusTag, diffLines) triple" $
  "name" ~> "ourContent" ~> "canonContent" ~>
  Optionals.cases (var "ourContent")
    (triple (var "name") (string "missing") (int32 0))
    ("ours" ~>
      Optionals.cases (var "canonContent")
        (triple (var "name") (string "missing") (int32 0))
        ("canon" ~>
          Logic.ifElse (Equality.equal (var "ours") (var "canon"))
            (triple (var "name") (string "byteEq") (int32 0))
            (triple (var "name") (string "differ") (diffLineCount @@ var "ours" @@ var "canon"))))

-- | A pure line-level difference count between two file contents: the number of
-- lines present in one side but not the other, counted symmetrically. This
-- replaces the native @diff@ subprocess the heredocs shelled out to for the
-- differing-line count. It is a set-difference count (order-insensitive), which is
-- sufficient for the report's "how different" signal; it is not a minimal edit
-- script. Lines are split on @"\n"@.
diffLineCount :: TypedTermDefinition (String -> String -> Int)
diffLineCount = define "diffLineCount" $
  doc "Symmetric count of lines present in one content but not the other" $
  "a" ~> "b" ~>
  "aLines" <~ Strings.splitOn (string "\n") (var "a") $
  "bLines" <~ Strings.splitOn (string "\n") (var "b") $
  "onlyA" <~ Lists.length (Lists.filter ("x" ~> Logic.not (Lists.member (var "x") (var "bLines"))) (var "aLines")) $
  "onlyB" <~ Lists.length (Lists.filter ("y" ~> Logic.not (Lists.member (var "y") (var "aLines"))) (var "bLines")) $
  Math.add (var "onlyA") (var "onlyB")

-- | The compared module names from a directory's raw entry list: keep the @*.json@
-- entries (via 'Walk.filterByExtension', which also orders them deterministically)
-- and strip the @.json@ suffix. This is the pure string core of the module
-- enumeration; the native executor supplies the raw @listDirectory@ entries.
moduleNamesFromEntries :: TypedTermDefinition ([String] -> [String])
moduleNamesFromEntries = define "moduleNamesFromEntries" $
  doc "The .json entries (sorted), with the .json suffix stripped, as module names" $
  "entries" ~>
  Lists.map ("e" ~> stripJsonSuffix @@ var "e")
    (Walk.filterByExtension @@ string "json" @@ var "entries")

-- | Whether the report passed: every compared module was byte-identical, i.e. the
-- @byteEqCount@ equals the @total@. Drives the native driver's exit code.
reportPassed :: TypedTermDefinition (([(String, String, Int)], Int, Int) -> Bool)
reportPassed = define "reportPassed" $
  doc "Whether every compared module was byte-identical (byteEqCount == total)" $
  "report" ~>
  Equality.equal (Pairs.first (Pairs.second (var "report"))) (Pairs.second (Pairs.second (var "report")))

-- | Render the report as the formatted summary text the drivers print: one line
-- per row (@"<name>: <status>[ (<n> lines differ)]"@) followed by the tally line
-- @"<byteEqCount>/<total> byte-identical"@. Pure over the report triple.
renderReport :: TypedTermDefinition (([(String, String, Int)], Int, Int) -> String)
renderReport = define "renderReport" $
  doc "The formatted per-module lines plus the byte-identical tally line" $
  "report" ~>
  "rows" <~ Pairs.first (var "report") $
  "byteEqCount" <~ Pairs.first (Pairs.second (var "report")) $
  "total" <~ Pairs.second (Pairs.second (var "report")) $
  "rowLines" <~ Lists.map ("r" ~> renderRow @@ var "r") (var "rows") $
  "summary" <~ Strings.concat (list [
      Literals.showInt32 (var "byteEqCount"),
      string "/",
      Literals.showInt32 (var "total"),
      string " byte-identical"]) $
  Strings.join (string "\n") (Lists.concat (list [var "rowLines", list [var "summary"]]))

-- | Render one @(moduleName, statusTag, diffLines)@ row to a line:
-- @"<name>: <status>"@, with @" (<n> lines differ)"@ appended only for a
-- @"differ"@ row. Pure over the triple.
renderRow :: TypedTermDefinition ((String, String, Int) -> String)
renderRow = define "renderRow" $
  doc "One report row rendered as a line: name: status[ (n lines differ)]" $
  "row" ~>
  "name" <~ Pairs.first (var "row") $
  "status" <~ Pairs.first (Pairs.second (var "row")) $
  "diffLines" <~ Pairs.second (Pairs.second (var "row")) $
  Logic.ifElse (Equality.equal (var "status") (string "differ"))
    (Strings.concat (list [
        var "name", string ": ", var "status",
        string " (", Literals.showInt32 (var "diffLines"), string " lines differ)"]))
    (Strings.concat (list [var "name", string ": ", var "status"]))

-- | Strip a trailing @.json@ from a file entry name, yielding the module name.
-- Implemented by splitting on @"."@ and rejoining all but the final segment with
-- @"."@ (so @"foo.bar.json"@ yields @"foo.bar"@). Total: an entry with no @"."@
-- yields itself unchanged (all-but-last of a one-element list is empty, so the
-- name is preserved by the length guard).
stripJsonSuffix :: TypedTermDefinition (String -> String)
stripJsonSuffix = define "stripJsonSuffix" $
  doc "Strip a trailing .json from an entry name, yielding the module name" $
  "entry" ~>
  "parts" <~ Strings.splitOn (string ".") (var "entry") $
  Logic.ifElse (Ordering.gt (Lists.length (var "parts")) (int32 1))
    (Strings.join (string ".") (Lists.take (Math.sub (Lists.length (var "parts")) (int32 1)) (var "parts")))
    (var "entry")
