module Hydra.Sources.Test.Lib.Effects where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
-- Effectful test cases are authored with HONESTLY-TYPED builders (Phantoms + Literals), NOT the
-- reified-Term builders in Hydra.Overlay.Haskell.Dsl.Typed.Terms. The reified builders (e.g. Terms.string,
-- Terms.primitive) construct hydra.core.Term *data* for the reduce/interpret path used by universal
-- tests; effectful cases instead compile directly to raw target effectful code, so their terms must
-- infer at their true types (effect<string>, string). For #494.
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms hiding ((++))  -- (@@), primitive, lambda, var, just, nothing, optional, wrap
import Hydra.Overlay.Haskell.Dsl.Typed.Literals               (string, binary)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.ByteString.Char8        as BC
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.File as File
import qualified Hydra.Lib.Effects as DefEffects
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Files as DefFiles
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Strings as DefStrings
import qualified Hydra.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.test.lib.effects"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core", ModuleName "hydra.file", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Effectful test cases for hydra.lib.effects primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.effects primitives. Two kinds of case live here:
--   * Pure effect programs (pure/map/apply/compose plus the result-only bind/foldl/mapList/
--     mapOptional cases): no file I/O, so a runner's per-case temp-directory scan detects no
--     hydra.lib.files primitives and skips directory preparation for them.
--   * Ordering-observation cases (the *Order / *Sequencing groups, #675): these make the
--     sequencing contract OBSERVABLE by appending to a single file inside a bind/foldList/mapList/
--     mapOptional chain and reading it back, so a host that reorders, double-runs, skips, or
--     eagerly runs an effect produces a different file and fails. These cases DO reference
--     hydra.lib.files, so the runner's per-case scan triggers temp-directory preparation for them.
-- The temp directory is the same canonical /tmp/hydra-testing used by hydra.test.lib.files.

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Effectful test cases for hydra.lib.effects primitives" $
    supergroup "hydra.lib.effects primitives" [
      effectsApply,
      effectsBind,
      effectsBindOrder,
      effectsCompose,
      effectsFoldl,
      effectsFoldListOrder,
      effectsMap,
      effectsMapList,
      effectsMapListOrder,
      effectsMapOptional,
      effectsMapOptionalOrder,
      effectsPure]

-- ============================================================================
-- Ordering-observation helpers and cases (#675).
-- These observe the effect-sequencing contract (order / exactly-once / deferral) by performing
-- REAL host file effects: append into one file inside an effect chain, then read it back. A pure
-- effect program cannot observe order (no side effect to reorder), so these are the enforcement
-- vehicle for the obligations specified in docs/specification/primitives/effects.md.
-- The helpers mirror hydra.test.lib.files (Files.hs) so both modules share the same idioms.
-- ============================================================================

testDir :: String
testDir = "/tmp/hydra-testing"

path :: String -> TypedTerm Term
path rel = wrap File._FilePath (string (testDir ++ "/" ++ rel))

bytes :: String -> TypedTerm Binary
bytes s = binary (BC.pack s)

-- Fold an effect<either<FileError, T>> into an effect<string> via the eithers eliminator, with the
-- right branch passed through a (T -> string) function and the left branch rendered as "ERR".
foldEither :: TypedTerm a -> TypedTerm b -> TypedTerm c
foldEither showRight eff = retype $
  primitive DefEffects.map
    @@ (lambda "r" $ primitive DefEithers.either
         @@ (lambda "_e" $ string "ERR")
         @@ (retype showRight)
         @@ var "r")
    @@ retype eff
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- Decode a binary value to a string (assuming valid UTF-8 here), folding the decode either via fromRight.
decodeBytes :: TypedTerm a -> TypedTerm b
decodeBytes b = retype $
  primitive DefEithers.either @@ (lambda "_e" $ string "<decode error>") @@ (lambda "s" $ var "s")
    @@ (primitive DefText.decodeUtf8 @@ retype b)
  where
    retype :: TypedTerm x -> TypedTerm y
    retype (TypedTerm t) = TypedTerm t

-- Read testDir/<rel> and decode to a string; "ERR" on a Left FileError.
readBack :: String -> TypedTerm a
readBack rel = foldEither (lambda "b" $ decodeBytes (var "b")) (primitive DefFiles.readFile @@ path rel)

-- bind: the first effect must be performed to completion before the second, each exactly once.
-- Vehicle: (write "") then append "A" then append "B", then read. Observed "AB" proves A-before-B,
-- once each. A host that ran the continuation first would observe "BA"; a double-run would observe
-- a repeated append; an eager/skipped effect would diverge.
effectsBindOrder :: TypedTerm TestGroup
effectsBindOrder = subgroup "bind (ordering)" [
  effectfulCase "bind performs the first effect before the second, each once"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "bind-order.txt" @@ bytes "")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefFiles.appendFile @@ path "bind-order.txt" @@ bytes "A")
            @@ (lambda "_a" $ primitive DefEffects.bind
                  @@ (primitive DefFiles.appendFile @@ path "bind-order.txt" @@ bytes "B")
                  @@ (lambda "_b" $ readBack "bind-order.txt"))))
    (string "AB")]

-- foldList: element effects performed left-to-right in list order, each exactly once.
-- Vehicle: append each list element to one file, threading the accumulator unchanged, then read.
-- Observed "ABC" proves left-to-right, once each.
effectsFoldListOrder :: TypedTerm TestGroup
effectsFoldListOrder = subgroup "foldList (ordering)" [
  effectfulCase "foldList performs element effects left-to-right, each once"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "fold-order.txt" @@ bytes "")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefEffects.foldList
                  @@ (lambda "acc" $ lambda "x" $ primitive DefEffects.bind
                        @@ (primitive DefFiles.appendFile @@ path "fold-order.txt" @@ var "x")
                        @@ (lambda "_a" $ primitive DefEffects.pure @@ var "acc"))
                  @@ string ""
                  @@ list [bytes "A", bytes "B", bytes "C"])
            @@ (lambda "_f" $ readBack "fold-order.txt")))
    (string "ABC")]

-- mapList: element effects performed left-to-right in list order, each exactly once.
-- Vehicle: append each list element to one file; ignore the collected results and read the file.
-- Observed "ABC" proves left-to-right, once each.
effectsMapListOrder :: TypedTerm TestGroup
effectsMapListOrder = subgroup "mapList (ordering)" [
  effectfulCase "mapList performs element effects left-to-right, each once"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "maplist-order.txt" @@ bytes "")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefEffects.mapList
                  @@ (lambda "x" $ primitive DefFiles.appendFile @@ path "maplist-order.txt" @@ var "x")
                  @@ list [bytes "A", bytes "B", bytes "C"])
            @@ (lambda "_m" $ readBack "maplist-order.txt")))
    (string "ABC")]

-- mapOptional: none performs no effect (zero appends); given performs exactly one effect.
-- Vehicle: seed the file with "seed", run mapOptional (append "X") over none / given, then read.
-- Observed "seed" (none) vs "seedX" (given) proves 0 vs exactly-1 appends.
effectsMapOptionalOrder :: TypedTerm TestGroup
effectsMapOptionalOrder = subgroup "mapOptional (ordering)" [
  effectfulCase "mapOptional over none performs no effect"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "mapopt-none.txt" @@ bytes "seed")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefEffects.mapOptional
                  @@ (lambda "_x" $ primitive DefFiles.appendFile @@ path "mapopt-none.txt" @@ bytes "X")
                  @@ (nothing :: TypedTerm (Maybe Binary)))
            @@ (lambda "_m" $ readBack "mapopt-none.txt")))
    (string "seed"),
  effectfulCase "mapOptional over a present value performs exactly one effect"
    (primitive DefEffects.bind
      @@ (primitive DefFiles.writeFile @@ path "mapopt-given.txt" @@ bytes "seed")
      @@ (lambda "_w" $ primitive DefEffects.bind
            @@ (primitive DefEffects.mapOptional
                  @@ (lambda "_x" $ primitive DefFiles.appendFile @@ path "mapopt-given.txt" @@ bytes "X")
                  @@ (just $ bytes "anything"))
            @@ (lambda "_m" $ readBack "mapopt-given.txt")))
    (string "seedX")]

-- pure : a -> effect<a>
effectsPure :: TypedTerm TestGroup
effectsPure = subgroup "pure" [
  effectfulCase "pure of a string yields the string"
    (primitive DefEffects.pure @@ string "hello")
    (string "hello")]

-- bind : effect<a> -> (a -> effect<b>) -> effect<b>
effectsBind :: TypedTerm TestGroup
effectsBind = subgroup "bind" [
  effectfulCase "bind threads a pure value into the next effect"
    (primitive DefEffects.bind
      @@ (primitive DefEffects.pure @@ string "abc")
      @@ (lambda "s" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "s" @@ string "!")))
    (string "abc!")]

-- map : (a -> b) -> effect<a> -> effect<b>
effectsMap :: TypedTerm TestGroup
effectsMap = subgroup "map" [
  effectfulCase "map applies a pure function to an effect result"
    (primitive DefEffects.map
      @@ (lambda "s" $ primitive DefStrings.concat2 @@ var "s" @@ string "-mapped")
      @@ (primitive DefEffects.pure @@ string "x"))
    (string "x-mapped")]

-- apply : effect<(a -> b)> -> effect<a> -> effect<b>
effectsApply :: TypedTerm TestGroup
effectsApply = subgroup "apply" [
  effectfulCase "apply applies an effectful function to an effectful argument"
    (primitive DefEffects.apply
      @@ (primitive DefEffects.pure @@ (lambda "s" $ primitive DefStrings.concat2 @@ string ">" @@ var "s"))
      @@ (primitive DefEffects.pure @@ string "y"))
    (string ">y")]

-- compose : (a -> effect<b>) -> (b -> effect<c>) -> a -> effect<c>
effectsCompose :: TypedTerm TestGroup
effectsCompose = subgroup "compose" [
  effectfulCase "compose runs two Kleisli arrows in sequence"
    (primitive DefEffects.compose
      @@ (lambda "a" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "a" @@ string "1"))
      @@ (lambda "b" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "b" @@ string "2"))
      @@ string "n")
    (string "n12")]

-- foldl : (a -> b -> effect<a>) -> a -> list<b> -> effect<a>
effectsFoldl :: TypedTerm TestGroup
effectsFoldl = subgroup "foldl" [
  effectfulCase "foldl sequences an effect-returning step over a list"
    (primitive DefEffects.foldList
      @@ (lambda "acc" $ lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "acc" @@ var "x"))
      @@ string ""
      @@ list [string "a", string "b", string "c"])
    (string "abc")]

-- mapList : (a -> effect<b>) -> list<a> -> effect<list<b>>
-- The resulting effect<list<string>> is mapped to a string via concatenation for comparison.
effectsMapList :: TypedTerm TestGroup
effectsMapList = subgroup "mapList" [
  effectfulCase "mapList applies an effect-returning function across a list and collects results"
    (primitive DefEffects.map
      @@ (lambda "xs" $ primitive DefStrings.concat @@ var "xs")
      @@ (primitive DefEffects.mapList
        @@ (lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "x" @@ string "."))
        @@ list [string "a", string "b"]))
    (string "a.b.")]

-- mapOptional : (a -> effect<b>) -> optional<a> -> effect<optional<b>>
-- The resulting effect<optional<string>> is mapped to a string via fromOptional for comparison.
effectsMapOptional :: TypedTerm TestGroup
effectsMapOptional = subgroup "mapOptional" [
  effectfulCase "mapOptional over a present value applies the function"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefOptionals.withDefault @@ string "<none>" @@ var "m")
      @@ (primitive DefEffects.mapOptional
        @@ (lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.concat2 @@ var "x" @@ string "!"))
        @@ (just $ string "present")))
    (string "present!"),
  effectfulCase "mapOptional over none yields none"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefOptionals.withDefault @@ string "<none>" @@ var "m")
      @@ (primitive DefEffects.mapOptional
        @@ (lambda "x" $ primitive DefEffects.pure @@ var "x")
        @@ nothing))
    (string "<none>")]
