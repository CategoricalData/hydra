-- | Primitive declarations for the hydra.core.lib.regex namespace.

module Hydra.Sources.Kernel.Lib.Regex where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.core.lib.regex"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.regex module.")}
  where
    definitions = [find, findAll, matches, replace, replaceAll, split]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Shared type aliases
ss :: Type
ss = Types.string Types.~> Types.string

str2 :: Type -> Type
str2 result = Types.string Types.~> Types.string Types.~> result

str3 :: Type -> Type
str3 result = Types.string Types.~> Types.string Types.~> Types.string Types.~> result

find :: PrimitiveDefinition
find = define "find" "Find the first regex match within a string, returning the matched substring if any."
  (sigWithParams [("pat", "the regex pattern to search for"), ("s", "the string to search within")] $ TypeScheme [] (str2 (Types.optional Types.string)) mempty)
  ["find(pat, s) returns Just(t) where t is the leftmost-longest substring of s matching pat, or\
  \ Nothing if pat does not match anywhere in s.",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; a pattern that does not parse under the Hydra regex grammar is treated as not matching\
  \ anywhere, the same as a well-formed pattern with no match."]

findAll :: PrimitiveDefinition
findAll = define "findAll" "Find all non-overlapping regex matches within a string."
  (sigWithParams [("pat", "the regex pattern to search for"), ("s", "the string to search within")] $ TypeScheme [] (str2 (Types.list Types.string)) mempty)
  ["findAll(pat, s) returns the list of all leftmost-longest, non-overlapping matches of pat in s, in\
  \ the order they appear. Returns the empty list if pat does not match anywhere.",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; an unparseable pattern yields the empty list, the same as a well-formed pattern with no\
  \ matches."]

matches :: PrimitiveDefinition
matches = define "matches" "Test whether a regex matches an entire string."
  (sigWithParams [("pat", "the regex pattern to test"), ("s", "the string to test against")] $ TypeScheme [] (str2 Types.boolean) mempty)
  ["matches(pat, s) returns true iff pat matches the whole of s (anchored at both ends; a match of a\
  \ proper substring of s does not suffice). To test whether pat matches anywhere within s, use find.",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; an unparseable pattern yields false, the same as a well-formed pattern with no match."]

replace :: PrimitiveDefinition
replace = define "replace" "Replace the first regex match in a string with a replacement string."
  (sigWithParams [("pat", "the regex pattern to match"), ("repl", "the replacement string"), ("s", "the string to operate on")] $ TypeScheme [] (str3 Types.string) mempty)
  ["replace(pat, repl, s) returns s with the first leftmost-longest match of pat replaced by repl. If\
  \ pat does not match, s is returned unchanged.",
   "Replacement-string syntax (capture-group references such as $1 or \\\\1, literal escapes) is\
  \ host-defined.",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; an unparseable pattern leaves s unchanged, the same as a well-formed pattern with no\
  \ match."]

replaceAll :: PrimitiveDefinition
replaceAll = define "replaceAll" "Replace all non-overlapping regex matches in a string with a replacement string."
  (sigWithParams [("pat", "the regex pattern to match"), ("repl", "the replacement string"), ("s", "the string to operate on")] $ TypeScheme [] (str3 Types.string) mempty)
  ["replaceAll(pat, repl, s) returns s with every leftmost-longest, non-overlapping match of pat\
  \ replaced by repl. If pat does not match anywhere, s is returned unchanged.",
   "Replacement-string syntax is host-defined; see replace for the caveat.",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; an unparseable pattern leaves s unchanged, the same as a well-formed pattern with no\
  \ match."]

split :: PrimitiveDefinition
split = define "split" "Split a string by occurrences of a regex pattern."
  (sigWithParams [("pat", "the regex pattern to split on"), ("s", "the string to split")] $ TypeScheme [] (str2 (Types.list Types.string)) mempty)
  ["split(pat, s) returns the list of substrings of s obtained by splitting on every leftmost-longest,\
  \ non-overlapping match of pat.",
   "Trailing empty splits are host-defined (some engines retain them, some discard them; for\
  \ portable code, do not rely on the trailing-empty behavior).",
   "Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.",
   "Total; an unparseable pattern yields the single-element list containing s, the same as a\
  \ well-formed pattern with no match."]
