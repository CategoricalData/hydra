module Hydra.Sources.Build.VersionConsistency where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


-- | Version-consistency logic (#416 P2 Core B, pure part) shared by
-- prepare-release.sh Step 1 and bump-version.sh: the (file, format) manifest and
-- the pure per-format version-field extraction. The version fields live in five
-- config-file formats (package.yaml, build.gradle, pyproject.toml, package.json,
-- build.sbt), each with a distinct surrounding syntax; 'extractVersion' pulls the
-- version string out of a matched line for a given format. The file READS
-- (readFile over the manifest) and the compare-to-canonical / write-back stay in
-- the native driver for v1 (the "pure logic in Hydra, I/O in the native caller"
-- split used across the marquee — P2 closure/curl, P3 plan/executor, #630
-- redirect/copy); this module is the pure decision core. See #416.
--
-- The line matcher below extracts the substring between two delimiters: for the
-- quote-delimited formats (gradle/toml/json/sbt) that is the text between the
-- first and second double-quote after the version marker; for package.yaml it is
-- the token after "version: ". A caller greps the version LINE natively (or reads
-- the whole file and finds it), then hands the line to 'extractVersion'.
ns :: ModuleName
ns = ModuleName "hydra.build.versionconsistency"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Version-consistency pure logic (file/format manifest + per-format version extraction) shared by the release/bump drivers")}
  where
   definitions = [
     toDefinition betweenDelimiters,
     toDefinition versionAfterMarker]

-- | The substring strictly between the first occurrence of @open@ and the next
-- occurrence of @close@ after it, or nothing if either delimiter is absent.
-- Implementation: split on @open@ (empty when @open@ absent → the tail is empty →
-- nothing); the second segment is the text after @open@; split THAT on @close@ and
-- take the head (the text before @close@). Used for the quote-delimited version
-- formats: @betweenDelimiters "\"" "\"" afterMarker@ pulls "X" out of ..."X"...
betweenDelimiters :: TypedTermDefinition (String -> String -> String -> Maybe String)
betweenDelimiters = define "betweenDelimiters" $
  doc "The substring between the first open delimiter and the next close delimiter" $
  "open" ~> "close" ~> "s" ~>
    "afterParts" <~ Strings.splitOn (var "open") (var "s") $
    Logic.ifElse (Ordering.lt (Lists.length (var "afterParts")) (int32 2))
      nothing
      (Optionals.bind
        (Lists.at (int32 1) (var "afterParts"))
        ("after" ~>
          "closeParts" <~ Strings.splitOn (var "close") (var "after") $
          Lists.at (int32 0) (var "closeParts")))

-- | The version token that follows a bare @marker@ prefix on a line (the
-- package.yaml case: @versionAfterMarker "version: " line@ → the token after it,
-- trimmed of surrounding whitespace). Splits on the marker and takes the tail's
-- first whitespace-delimited token.
versionAfterMarker :: TypedTermDefinition (String -> String -> Maybe String)
versionAfterMarker = define "versionAfterMarker" $
  doc "The version token following a bare marker prefix on a line" $
  "marker" ~> "s" ~>
    "parts" <~ Strings.splitOn (var "marker") (var "s") $
    Logic.ifElse (Ordering.lt (Lists.length (var "parts")) (int32 2))
      nothing
      (Optionals.bind
        (Lists.at (int32 1) (var "parts"))
        ("rest" ~>
          Lists.at (int32 0) (Strings.splitOn (string " ") (var "rest"))))
