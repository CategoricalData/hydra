-- | Primitive declarations for the hydra.lib.chars namespace.

module Hydra.Sources.Kernel.Lib.Chars where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.chars"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.chars namespace."}
  where
    definitions = [
      primNoDef "isAlphaNum" "Check whether a character is alphanumeric." intToBoolSig Nothing,
      primNoDef "isLower"    "Check whether a character is lowercase." intToBoolSig Nothing,
      primNoDef "isSpace"    "Check whether a character is a whitespace character." intToBoolSig Nothing,
      primNoDef "isUpper"    "Check whether a character is uppercase." intToBoolSig Nothing,
      primNoDef "toLower"    "Convert a character to lowercase." intToIntSig Nothing,
      primNoDef "toUpper"    "Convert a character to uppercase." intToIntSig Nothing]

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Shared signatures.

-- Int32 -> Boolean
intToBoolSig :: TermSignature
intToBoolSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing

-- Int32 -> Int32
intToIntSig :: TermSignature
intToIntSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.int32) Nothing
