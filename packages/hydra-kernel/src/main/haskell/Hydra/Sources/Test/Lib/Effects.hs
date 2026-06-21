module Hydra.Sources.Test.Lib.Effects where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
-- Effectful test cases are authored with HONESTLY-TYPED builders (Phantoms + Literals), NOT the
-- reified-Term builders in Hydra.Dsl.Meta.Terms. The reified builders (e.g. Terms.string,
-- Terms.primitive) construct hydra.core.Term *data* for the reduce/interpret path used by universal
-- tests; effectful cases instead compile directly to raw target effectful code, so their terms must
-- infer at their true types (effect<string>, string). For #494.
import Hydra.Dsl.Meta.Phantoms               -- (@@), primitive, lambda, var, just, nothing, optional, wrap
import Hydra.Dsl.Meta.Literals               (string)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Lib.Effects as DefEffects
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.test.lib.effects"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Effectful test cases for hydra.lib.effects primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.effects primitives. These are pure effect programs (no file I/O),
-- so a test runner's per-case temp-directory scan should detect no hydra.lib.files primitives and
-- skip directory preparation entirely.

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Effectful test cases for hydra.lib.effects primitives" $
    supergroup "hydra.lib.effects primitives" [
      effectsApply,
      effectsBind,
      effectsCompose,
      effectsFoldl,
      effectsMap,
      effectsMapList,
      effectsMapOptional,
      effectsPure]

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
      @@ (lambda "s" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "s" @@ string "!")))
    (string "abc!")]

-- map : (a -> b) -> effect<a> -> effect<b>
effectsMap :: TypedTerm TestGroup
effectsMap = subgroup "map" [
  effectfulCase "map applies a pure function to an effect result"
    (primitive DefEffects.map
      @@ (lambda "s" $ primitive DefStrings.cat2 @@ var "s" @@ string "-mapped")
      @@ (primitive DefEffects.pure @@ string "x"))
    (string "x-mapped")]

-- apply : effect<(a -> b)> -> effect<a> -> effect<b>
effectsApply :: TypedTerm TestGroup
effectsApply = subgroup "apply" [
  effectfulCase "apply applies an effectful function to an effectful argument"
    (primitive DefEffects.apply
      @@ (primitive DefEffects.pure @@ (lambda "s" $ primitive DefStrings.cat2 @@ string ">" @@ var "s"))
      @@ (primitive DefEffects.pure @@ string "y"))
    (string ">y")]

-- compose : (a -> effect<b>) -> (b -> effect<c>) -> a -> effect<c>
effectsCompose :: TypedTerm TestGroup
effectsCompose = subgroup "compose" [
  effectfulCase "compose runs two Kleisli arrows in sequence"
    (primitive DefEffects.compose
      @@ (lambda "a" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "a" @@ string "1"))
      @@ (lambda "b" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "b" @@ string "2"))
      @@ string "n")
    (string "n12")]

-- foldl : (a -> b -> effect<a>) -> a -> list<b> -> effect<a>
effectsFoldl :: TypedTerm TestGroup
effectsFoldl = subgroup "foldl" [
  effectfulCase "foldl sequences an effect-returning step over a list"
    (primitive DefEffects.foldl
      @@ (lambda "acc" $ lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "acc" @@ var "x"))
      @@ string ""
      @@ list [string "a", string "b", string "c"])
    (string "abc")]

-- mapList : (a -> effect<b>) -> list<a> -> effect<list<b>>
-- The resulting effect<list<string>> is mapped to a string via concatenation for comparison.
effectsMapList :: TypedTerm TestGroup
effectsMapList = subgroup "mapList" [
  effectfulCase "mapList applies an effect-returning function across a list and collects results"
    (primitive DefEffects.map
      @@ (lambda "xs" $ primitive DefStrings.cat @@ var "xs")
      @@ (primitive DefEffects.mapList
        @@ (lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "x" @@ string "."))
        @@ list [string "a", string "b"]))
    (string "a.b.")]

-- mapOptional : (a -> effect<b>) -> optional<a> -> effect<optional<b>>
-- The resulting effect<optional<string>> is mapped to a string via fromOptional for comparison.
effectsMapOptional :: TypedTerm TestGroup
effectsMapOptional = subgroup "mapOptional" [
  effectfulCase "mapOptional over a present value applies the function"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefOptionals.fromOptional @@ string "<none>" @@ var "m")
      @@ (primitive DefEffects.mapOptional
        @@ (lambda "x" $ primitive DefEffects.pure @@ (primitive DefStrings.cat2 @@ var "x" @@ string "!"))
        @@ (just $ string "present")))
    (string "present!"),
  effectfulCase "mapOptional over none yields none"
    (primitive DefEffects.map
      @@ (lambda "m" $ primitive DefOptionals.fromOptional @@ string "<none>" @@ var "m")
      @@ (primitive DefEffects.mapOptional
        @@ (lambda "x" $ primitive DefEffects.pure @@ var "x")
        @@ nothing))
    (string "<none>")]
