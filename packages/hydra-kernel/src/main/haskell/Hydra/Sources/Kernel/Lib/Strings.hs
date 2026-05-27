-- | Primitive declarations for the hydra.lib.strings namespace.

module Hydra.Sources.Kernel.Lib.Strings where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.strings"

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
            moduleDescription = Just "Primitives in the hydra.lib.strings namespace."}
  where
    definitions = [
      primNoDef "cat"
        "Concatenate a list of strings into a single string."
        (sigFn (Types.list Types.string) Types.string),
      primNoDef "cat2"
        "Concatenate two strings."
        (sigFn2 Types.string Types.string Types.string),
      primNoDef "fromList"
        "Convert a list of Unicode code points to a string."
        (sigFn (Types.list Types.int32) Types.string),
      primNoDef "intercalate"
        "Join a list of strings with a separator between each element."
        (sigFn2 Types.string (Types.list Types.string) Types.string),
      primNoDef "length"
        "Return the length of a string."
        (sigFn Types.string Types.int32),
      primNoDef "lines"
        "Split a string into lines."
        (sigFn Types.string (Types.list Types.string)),
      primNoDef "maybeCharAt"
        "Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds."
        (sigFn2 Types.int32 Types.string (Types.optional Types.int32)),
      primNoDef "null"
        "Check whether a string is empty."
        (sigFn Types.string Types.boolean),
      primNoDef "splitOn"
        "Split a string on a delimiter string."
        (sigFn2 Types.string Types.string (Types.list Types.string)),
      primNoDef "toList"
        "Convert a string to a list of Unicode code points."
        (sigFn Types.string (Types.list Types.int32)),
      primNoDef "toLower"
        "Convert a string to lowercase."
        (sigFn Types.string Types.string),
      primNoDef "toUpper"
        "Convert a string to uppercase."
        (sigFn Types.string Types.string),
      primNoDef "unlines"
        "Join a list of strings with newlines, appending a trailing newline."
        (sigFn (Types.list Types.string) Types.string)]

-- Helpers: build TermSignatures from monomorphic function types.

sigFn :: Type -> Type -> TermSignature
sigFn a b = sig $ TypeScheme [] (a Types.~> b) Nothing

sigFn2 :: Type -> Type -> Type -> TermSignature
sigFn2 a b c = sig $ TypeScheme [] (a Types.~> b Types.~> c) Nothing
