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
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.regex module.")}
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
  (sig $ TypeScheme [] (str2 (Types.optional Types.string)) Nothing)
  ["find(pat, s) returns Just(t) where t is the leftmost substring of s matching pat, or Nothing if\
  \ pat does not match anywhere in s.",
   "Regex syntax is host-defined; behavior tends to converge on the intersection of ECMA-262 and\
  \ POSIX-ERE features (literal characters, character classes, alternation, anchors ^ and $,\
  \ repetition with ?/*/+/{n,m}, grouping with parentheses), but extension features (lookaround,\
  \ backreferences, Unicode property classes, named groups, engine-specific flags) vary widely.\
  \ For portable code, restrict patterns to the common subset.",
   "Total in the sense that no error is raised at the kernel level; behavior on an ill-formed\
  \ pattern is host-defined."]

findAll :: PrimitiveDefinition
findAll = define "findAll" "Find all non-overlapping regex matches within a string."
  (sig $ TypeScheme [] (str2 (Types.list Types.string)) Nothing)
  ["findAll(pat, s) returns the list of all leftmost, non-overlapping matches of pat in s, in the\
  \ order they appear. Returns the empty list if pat does not match anywhere.",
   "Regex syntax is host-defined; see find for the common-subset caveat.",
   "Total; ill-formed patterns are host-defined."]

matches :: PrimitiveDefinition
matches = define "matches" "Test whether a regex matches anywhere in a string."
  (sig $ TypeScheme [] (str2 Types.boolean) Nothing)
  ["matches(pat, s) returns true iff pat matches somewhere in s (not anchored to the start or end;\
  \ for whole-string matching, anchor the pattern explicitly with ^ and $).",
   "Regex syntax is host-defined; see find for the common-subset caveat.",
   "Total; ill-formed patterns are host-defined."]

replace :: PrimitiveDefinition
replace = define "replace" "Replace the first regex match in a string with a replacement string."
  (sig $ TypeScheme [] (str3 Types.string) Nothing)
  ["replace(pat, repl, s) returns s with the first leftmost match of pat replaced by repl. If pat\
  \ does not match, s is returned unchanged.",
   "Replacement-string syntax (capture-group references such as $1 or \\\\1, literal escapes) is\
  \ host-defined.",
   "Regex syntax is host-defined; see find for the common-subset caveat.",
   "Total; ill-formed patterns and replacement strings are host-defined."]

replaceAll :: PrimitiveDefinition
replaceAll = define "replaceAll" "Replace all non-overlapping regex matches in a string with a replacement string."
  (sig $ TypeScheme [] (str3 Types.string) Nothing)
  ["replaceAll(pat, repl, s) returns s with every leftmost, non-overlapping match of pat replaced\
  \ by repl. If pat does not match anywhere, s is returned unchanged.",
   "Replacement-string syntax is host-defined; see replace and find for the common-subset caveats.",
   "Total; ill-formed patterns and replacement strings are host-defined."]

split :: PrimitiveDefinition
split = define "split" "Split a string by occurrences of a regex pattern."
  (sig $ TypeScheme [] (str2 (Types.list Types.string)) Nothing)
  ["split(pat, s) returns the list of substrings of s obtained by splitting on every leftmost,\
  \ non-overlapping match of pat.",
   "Trailing empty splits are host-defined (some engines retain them, some discard them; for\
  \ portable code, do not rely on the trailing-empty behavior).",
   "Regex syntax is host-defined; see find for the common-subset caveat.",
   "Total; ill-formed patterns are host-defined."]
