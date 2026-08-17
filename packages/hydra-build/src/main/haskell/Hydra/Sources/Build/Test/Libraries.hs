-- | Test cases for the translingual expected-libraries registry in
-- hydra.build.libraries (#533).
--
-- These are cross-host behavioral tests for 'isExpectedLibrary' and the shape of
-- 'expectedLibraries' -- they run on every self-hosting target via the hydra-kernel
-- test suite. The single-source-of-truth GUARD (comparing the registry's content
-- against the kernel's actual Lib/*.hs file set) is a separate, Haskell-only check:
-- see Hydra.BuildLibrariesSpec in heads/haskell, which can read the filesystem.
module Hydra.Sources.Build.Test.Libraries where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))

import qualified Hydra.Sources.Build.Libraries as BuildLibraries
import qualified Hydra.Dsl.Lib.Logic as Logic
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore


ns :: ModuleName
ns = ModuleName "hydra.test.build.libraries"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([BuildLibraries.ns, PrintCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for the translingual expected-libraries registry")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Show a list of strings.
idStr :: TypedTerm (String -> String)
idStr = Phantoms.lambda "s" (Phantoms.var "s")

showStrs :: TypedTerm [String] -> TypedTerm String
showStrs xs = PrintCore.list_ @@ idStr @@ xs

-- Show a boolean as "true" / "false".
showBool :: TypedTerm Bool -> TypedTerm String
showBool b = Logic.ifElse b (Phantoms.string "true") (Phantoms.string "false")

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for the translingual expected-libraries registry" $
    supergroup "build.libraries" [
      expectedLibrariesGroup,
      isExpectedLibraryGroup]

-- expectedLibraries: a fixed, non-empty, alphabetically-ordered list. The full
-- 20-entry content is pinned once here; per-host completeness against the kernel's
-- actual Lib/*.hs files is the Haskell-only guard in Hydra.BuildLibrariesSpec.
expectedLibrariesGroup :: TypedTerm TestGroup
expectedLibrariesGroup = subgroup "expectedLibraries" [
    universalCase "the full 20-entry registry, in alphabetical order"
      (showStrs (Phantoms.asTerm BuildLibraries.expectedLibraries))
      (showStrs (Phantoms.list (Phantoms.string <$> [
        "chars", "effects", "eithers", "equality", "files", "functions", "hashing",
        "lists", "literals", "logic", "maps", "math", "optionals", "ordering",
        "pairs", "regex", "sets", "strings", "system", "text"])))]

-- isExpectedLibrary: true for every registered sub, false for anything else.
isExpectedLibraryGroup :: TypedTerm TestGroup
isExpectedLibraryGroup = subgroup "isExpectedLibrary" [
    universalCase "a registered sub (chars)"
      (showBool (BuildLibraries.isExpectedLibrary @@ Phantoms.string "chars"))
      (showBool Phantoms.true),
    universalCase "a registered effectful sub (hashing)"
      (showBool (BuildLibraries.isExpectedLibrary @@ Phantoms.string "hashing"))
      (showBool Phantoms.true),
    universalCase "an unregistered sub (defaults is deliberately excluded)"
      (showBool (BuildLibraries.isExpectedLibrary @@ Phantoms.string "defaults"))
      (showBool Phantoms.false),
    universalCase "a made-up sub"
      (showBool (BuildLibraries.isExpectedLibrary @@ Phantoms.string "notasub"))
      (showBool Phantoms.false),
    universalCase "the empty string"
      (showBool (BuildLibraries.isExpectedLibrary @@ Phantoms.string ""))
      (showBool Phantoms.false)]
