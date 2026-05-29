-- | Primitive declarations for the hydra.lib.regex namespace.

module Hydra.Sources.Kernel.Lib.Regex where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.regex"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.regex namespace."}
  where
    definitions = [
      primNoDef "find"
        "Find the first regex match within a string, returning the matched substring if any."
        ssToOptStrSig Nothing,
      primNoDef "findAll"
        "Find all non-overlapping regex matches within a string."
        ssToListStrSig Nothing,
      primNoDef "matches"
        "Test whether a regex matches anywhere in a string."
        ssToBoolSig Nothing,
      primNoDef "replace"
        "Replace the first regex match in a string with a replacement string."
        sssToStrSig Nothing,
      primNoDef "replaceAll"
        "Replace all non-overlapping regex matches in a string with a replacement string."
        sssToStrSig Nothing,
      primNoDef "split"
        "Split a string by occurrences of a regex pattern."
        ssToListStrSig Nothing]

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Shared signatures (all 6 prims operate on plain strings, no type vars).

-- pattern -> input -> boolean
ssToBoolSig :: TermSignature
ssToBoolSig = sig $ TypeScheme []
  (Types.string Types.~> Types.string Types.~> Types.boolean)
  Nothing

-- pattern -> input -> list<matched-string>
ssToListStrSig :: TermSignature
ssToListStrSig = sig $ TypeScheme []
  (Types.string Types.~> Types.string Types.~> Types.list Types.string)
  Nothing

-- pattern -> input -> optional matched-string
ssToOptStrSig :: TermSignature
ssToOptStrSig = sig $ TypeScheme []
  (Types.string Types.~> Types.string Types.~> Types.optional Types.string)
  Nothing

-- pattern -> replacement -> input -> output string
sssToStrSig :: TermSignature
sssToStrSig = sig $ TypeScheme []
  (Types.string Types.~> Types.string Types.~> Types.string Types.~> Types.string)
  Nothing
