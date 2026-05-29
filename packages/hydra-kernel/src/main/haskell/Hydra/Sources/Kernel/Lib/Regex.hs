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
        ssToOptStrSig (Just
          "find(pat, s) returns Just(t) where t is the leftmost substring of s matching pat, or Nothing if\
          \ pat does not match anywhere in s. Regex syntax is host-defined; behavior tends to converge on\
          \ the intersection of ECMA-262 and POSIX-ERE features (literal characters, character classes,\
          \ alternation, anchors ^ and $, repetition with ?/*/+/{n,m}, grouping with parentheses), but\
          \ extension features (lookaround, backreferences, Unicode property classes, named groups,\
          \ engine-specific flags) vary widely. For portable code, restrict patterns to the common\
          \ subset. Total in the sense that no error is raised at the kernel level; behavior on an\
          \ ill-formed pattern is host-defined."),
      primNoDef "findAll"
        "Find all non-overlapping regex matches within a string."
        ssToListStrSig (Just
          "findAll(pat, s) returns the list of all leftmost, non-overlapping matches of pat in s, in the\
          \ order they appear. Returns the empty list if pat does not match anywhere. Regex syntax is\
          \ host-defined; see find for the common-subset caveat. Total; ill-formed patterns are\
          \ host-defined."),
      primNoDef "matches"
        "Test whether a regex matches anywhere in a string."
        ssToBoolSig (Just
          "matches(pat, s) returns true iff pat matches somewhere in s (not anchored to the start or end;\
          \ for whole-string matching, anchor the pattern explicitly with ^ and $). Regex syntax is\
          \ host-defined; see find for the common-subset caveat. Total; ill-formed patterns are\
          \ host-defined."),
      primNoDef "replace"
        "Replace the first regex match in a string with a replacement string."
        sssToStrSig (Just
          "replace(pat, repl, s) returns s with the first leftmost match of pat replaced by repl. If pat\
          \ does not match, s is returned unchanged. Replacement-string syntax (capture-group references\
          \ such as $1 or \\\\1, literal escapes) is host-defined. Regex syntax is host-defined; see find\
          \ for the common-subset caveat. Total; ill-formed patterns and replacement strings are\
          \ host-defined."),
      primNoDef "replaceAll"
        "Replace all non-overlapping regex matches in a string with a replacement string."
        sssToStrSig (Just
          "replaceAll(pat, repl, s) returns s with every leftmost, non-overlapping match of pat replaced\
          \ by repl. If pat does not match anywhere, s is returned unchanged. Replacement-string syntax is\
          \ host-defined; see replace and find for the common-subset caveats. Total; ill-formed patterns\
          \ and replacement strings are host-defined."),
      primNoDef "split"
        "Split a string by occurrences of a regex pattern."
        ssToListStrSig (Just
          "split(pat, s) returns the list of substrings of s obtained by splitting on every leftmost,\
          \ non-overlapping match of pat. Trailing empty splits are host-defined (some engines retain\
          \ them, some discard them; for portable code, do not rely on the trailing-empty behavior).\
          \ Regex syntax is host-defined; see find for the common-subset caveat. Total; ill-formed\
          \ patterns are host-defined.")]

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
