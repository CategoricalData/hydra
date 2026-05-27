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

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.chars namespace."}
  where
    definitions = [
      primNoDef "isAlphaNum" "Check whether a character is alphanumeric." intToBoolSig,
      primNoDef "isLower"    "Check whether a character is lowercase." intToBoolSig,
      primNoDef "isSpace"    "Check whether a character is a whitespace character." intToBoolSig,
      primNoDef "isUpper"    "Check whether a character is uppercase." intToBoolSig,
      primNoDef "toLower"    "Convert a character to lowercase." intToIntSig,
      primNoDef "toUpper"    "Convert a character to uppercase." intToIntSig]

-- Shared signatures.

-- Int32 -> Boolean
intToBoolSig :: TermSignature
intToBoolSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing

-- Int32 -> Int32
intToIntSig :: TermSignature
intToIntSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.int32) Nothing
