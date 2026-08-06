
module Hydra.Sources.Build.LibraryRedirect where

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
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Ordering as Ordering
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


-- | Driver-level correction of the coders' shape-only @hydra.lib.*@ overlay
-- redirect (#568, #416 Step A promotion of the lib-redirect logic from
-- @Hydra.Generation@).
--
-- The DSL-level coders (Haskell, TypeScript) redirect EVERY 3-segment
-- @hydra.lib.\<sub\>@ reference to the host overlay path unconditionally --
-- shape-only, because the DSL has no I/O and cannot check whether a given sub
-- actually has an overlay implementation on disk. This module is the
-- translingual port of the corrective rewrite that NARROWS that redirect back
-- down to only the subs that really have an overlay: given the set of
-- overlay-backed sub names (discovered on disk by the native driver's
-- @overlayLibSubs@, which stays host-native -- it is filesystem I/O), rewrite
-- every redirected reference whose sub is NOT overlay-backed from the overlay
-- prefix back to the canonical kernel prefix, leaving the overlay-backed ones
-- redirected.
--
-- 'redirectLibBack' is the shared pure rewrite; 'correctHaskellLibRedirect' and
-- 'correctTypeScriptLibRedirect' bind the two host-specific prefix pairs. The
-- native @Hydra.Generation@ correctHaskell/TypeScriptLibRedirect become thin
-- delegations to these, converting their on-disk @Set String@ of known subs to
-- the @[String]@ this module consumes (mirroring the #559 Step E
-- packaging-profile split: pure policy here, filesystem I/O in the driver).
--
-- Behavior contract (pinned by heads/haskell .../GenerationLibRedirectSpec.hs):
-- an occurrence of @old@ is rewritten only when immediately followed by an
-- identifier run (the sub name); the sub is matched case-INSENSITIVELY against
-- the known set, while @old@ itself is matched case-SENSITIVELY (the lowercase
-- dotted namespace form never appears on the wire and must not be treated as a
-- redirect); every occurrence is handled independently and non-matching text is
-- left untouched.
ns :: ModuleName
ns = ModuleName "hydra.build.libraryRedirect"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Driver-level correction of the coders' shape-only hydra.lib.* overlay redirect: narrow it to only overlay-backed subs")}
  where
   definitions = [
     toDefinition correctHaskellLibRedirect,
     toDefinition correctTypeScriptLibRedirect,
     toDefinition isLibIdentChar,
     toDefinition redirectLibBack,
     toDefinition takeLibIdentPrefix]

-- | Narrow the Haskell coder's shape-only @Hydra.Overlay.Haskell.Lib.\<Sub\>@
-- redirect back to the canonical @Hydra.Lib.\<Sub\>@ kernel module for every sub
-- NOT in the overlay-backed set. Thin binding of 'redirectLibBack' with the
-- Haskell host's PascalCase prefix pair (the Haskell coder capitalizes every
-- dotted segment when building the qualified import name, so the on-the-wire
-- text is @Hydra.Overlay.Haskell.Lib.Defaults@, never the lowercase dotted
-- form). No-op for subs that DO have an overlay -- those stay redirected.
correctHaskellLibRedirect :: TypedTermDefinition ([String] -> String -> String)
correctHaskellLibRedirect = define "correctHaskellLibRedirect" $
  doc "Narrow the Haskell coder's shape-only hydra.lib.* overlay redirect back to only overlay-backed subs" $
  "knownSubs" ~>
  redirectLibBack @@ var "knownSubs" @@ string "Hydra.Overlay.Haskell.Lib." @@ string "Hydra.Lib."

-- | TypeScript analog of 'correctHaskellLibRedirect': narrow the TS coder's
-- shape-only @overlay/typescript/lib/\<sub\>@ redirect back to the canonical
-- @lib/\<sub\>@ def-module path for every sub NOT in the overlay-backed set.
correctTypeScriptLibRedirect :: TypedTermDefinition ([String] -> String -> String)
correctTypeScriptLibRedirect = define "correctTypeScriptLibRedirect" $
  doc "Narrow the TypeScript coder's shape-only hydra.lib.* overlay redirect back to only overlay-backed subs" $
  "knownSubs" ~>
  redirectLibBack @@ var "knownSubs" @@ string "overlay/typescript/lib/" @@ string "lib/"

-- | Whether a single character (as a Unicode code point) is an identifier
-- character for the purpose of delimiting a lib sub name: @_@, a digit, or an
-- ASCII letter. Matches the native @isIdentChar@ used by the original
-- 'redirectLibBack' scanner.
isLibIdentChar :: TypedTermDefinition (Int -> Bool)
isLibIdentChar = define "isLibIdentChar" $
  doc "Whether a code point is an identifier character (underscore, digit, or ASCII letter)" $
  "c" ~>
  Logic.or (Equality.equal (var "c") (int32 95)) $  -- '_'
  Logic.or (Logic.and (Ordering.gte (var "c") (int32 48)) (Ordering.lte (var "c") (int32 57))) $  -- '0'..'9'
  Logic.or (Logic.and (Ordering.gte (var "c") (int32 65)) (Ordering.lte (var "c") (int32 90)))    -- 'A'..'Z'
           (Logic.and (Ordering.gte (var "c") (int32 97)) (Ordering.lte (var "c") (int32 122)))   -- 'a'..'z'

-- | The shared corrective rewrite. @redirectLibBack knownSubs old new s@ scans
-- @s@ for every occurrence of @old@ immediately followed by an identifier run
-- (the sub name); for each occurrence whose sub (lowercased) is NOT in
-- @knownSubs@, that occurrence's prefix is rewritten from @old@ to @new@;
-- occurrences whose sub IS known are left as @old@. Only narrows what the DSL
-- coder already emitted -- never introduces a new redirect.
--
-- Implementation: @Strings.splitOn old@ partitions @s@ so that the first
-- segment is the text before any occurrence of @old@ (emitted verbatim) and
-- every subsequent segment is the text that immediately FOLLOWED an occurrence
-- of @old@ (i.e. begins with the sub name, if any). For each subsequent segment
-- the leading identifier run is the sub; if it is empty, or its lowercase form
-- is in @knownSubs@, the occurrence keeps @old@; otherwise it becomes @new@.
-- Segments are rejoined in order. This is behavior-equivalent to the original
-- character-recursive @takeWhile isIdentChar@ scanner, including the case where
-- @old@ is followed by a non-identifier character (empty sub -> keep @old@).
redirectLibBack :: TypedTermDefinition ([String] -> String -> String -> String -> String)
redirectLibBack = define "redirectLibBack" $
  doc "Rewrite each occurrence of old (followed by an unknown-sub identifier) to new; leave known subs redirected" $
  "knownSubs" ~> "old" ~> "new" ~> "s" ~>
  "segs" <~ (Strings.splitOn (var "old") (var "s") :: TypedTerm [String]) $
  Optionals.cases (Lists.uncons (var "segs"))
    (var "s")  -- splitOn never returns [], but be total: no occurrence -> original
    ("uc" ~>
      "firstSeg" <~ Pairs.first (var "uc") $
      "rest" <~ (Pairs.second (var "uc") :: TypedTerm [String]) $
      -- Each rest-segment followed an occurrence of `old`. Prefix it with the
      -- corrected prefix (old kept, or rewritten to new) then the segment itself.
      "rewritten" <~ (Lists.map
        ("seg" ~>
          "sub" <~ (takeLibIdentPrefix @@ var "seg" :: TypedTerm String) $
          Strings.concat (list [
            Logic.ifElse
              (Logic.and (Logic.not (Strings.null (var "sub")))
                         (Logic.not (Lists.member (Strings.toLower (var "sub")) (var "knownSubs" :: TypedTerm [String]))))
              (var "new")
              (var "old"),
            var "seg"]))
        (var "rest") :: TypedTerm [String]) $
      Strings.concat (Lists.cons (var "firstSeg") (var "rewritten")))

-- | The leading identifier run of a string: the longest prefix consisting only
-- of 'isLibIdentChar' characters. Empty if the first character is not an
-- identifier character (or the string is empty). Translingual replacement for
-- @takeWhile isIdentChar@, expressed over the code-point list because the
-- available string primitives offer no @takeWhile@ / substring operation.
takeLibIdentPrefix :: TypedTermDefinition (String -> String)
takeLibIdentPrefix = define "takeLibIdentPrefix" $
  doc "The leading identifier-character run of a string (empty if it does not start with one)" $
  "s" ~>
  "cps" <~ (Strings.toList (var "s") :: TypedTerm [Int]) $
  -- Fold left accumulating code points while still in the leading identifier run.
  -- Carry (stillTaking, acc-reversed) to stop at the first non-identifier char.
  "stepped" <~ (Lists.foldl
    ("acc" ~> "c" ~>
      "taking" <~ Pairs.first (var "acc" :: TypedTerm (Bool, [Int])) $
      "taken" <~ (Pairs.second (var "acc") :: TypedTerm [Int]) $
      Logic.ifElse (Logic.and (var "taking") (isLibIdentChar @@ var "c"))
        (pair true (Lists.cons (var "c") (var "taken")))
        (pair false (var "taken")))
    (pair true (list ([] :: [TypedTerm Int])))
    (var "cps") :: TypedTerm (Bool, [Int])) $
  Strings.fromList (Lists.reverse (Pairs.second (var "stepped")))
