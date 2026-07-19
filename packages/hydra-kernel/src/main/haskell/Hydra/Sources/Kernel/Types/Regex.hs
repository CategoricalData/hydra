module Hydra.Sources.Kernel.Types.Regex where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T


ns :: ModuleName
ns = ModuleName "hydra.regex"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata (Just $
              "A model for Hydra's translingual regular-expression syntax: the abstract syntax tree"
              ++ " (AST) into which hydra.parse.regex parses a pattern and from which hydra.print.regex"
              ++ " (and the per-dialect hydra.print.<dialect>.regex renderers) emit target syntax."
              ++ " Covers the minimal core defined in docs/specification/regex.md (literals, character"
              ++ " classes, ., quantifiers, alternation, anchors, grouping). See issue #567.")}
  where
    definitions = [
      alternation,
      atom,
      characterClass,
      characterRange,
      classItem,
      quantified,
      quantifier,
      quantifierRange,
      regex,
      sequence']

-- Definitions are listed alphabetically, per the coding-style rules.

alternation :: TypeDefinition
alternation = define "Alternation" $
  doc ("A non-empty list of alternative sequences, joined by the | operator. Matches if any"
    ++ " alternative matches.") $
  T.list sequence'

atom :: TypeDefinition
atom = define "Atom" $
  doc "A single, unquantified regex element: the smallest unit to which a quantifier may apply." $
  T.union [
    "literal">:
      doc ("A literal character, matched exactly. The character is a Unicode scalar value in the range"
        ++ " [U+0000, U+10FFFF] (surrogates excluded), held as an int32 code point (the range fits in"
        ++ " signed 32-bit, and int32 matches Hydra's string/parser code-point representation). In"
        ++ " concrete syntax, a metacharacter is written escaped (e.g. \\.); escaping is purely a"
        ++ " concrete-syntax concern, so the AST holds the bare code point.") $
      T.int32,
    "any">:
      doc ("The . metacharacter; matches any single character, INCLUDING newline (unlike most host"
        ++ " engines, whose native . excludes newline). To exclude newline, write [^\\n] explicitly.")
      T.unit,
    "anchorStart">:
      doc "The ^ anchor; matches the empty string at the start of input." T.unit,
    "anchorEnd">:
      doc "The $ anchor; matches the empty string at the end of input." T.unit,
    "group">:
      doc "A parenthesized sub-expression, ( ... ); groups an alternation for quantification."
      alternation,
    "class">:
      doc "A character class, [ ... ]; matches any single character in (or, if negated, not in) the set."
      characterClass]

characterClass :: TypeDefinition
characterClass = define "CharacterClass" $
  doc "A bracketed character class, [ ... ] or [^ ... ]." $
  T.record [
    "negated">:
      doc "True for a negated class ([^ ... ]), which matches any character NOT listed." $
      T.boolean,
    "items">:
      doc ("The class members: individual characters and/or character ranges. Must be non-empty; the"
        ++ " empty class [] and negated-empty class [^] are excluded from the core (they are not in the"
        ++ " POSIX/PCRE/ECMA intersection). hydra.parse.regex rejects an empty class.") $
      T.list classItem]

characterRange :: TypeDefinition
characterRange = define "CharacterRange" $
  doc "An inclusive range of characters within a character class, e.g. a-z." $
  T.record [
    "from">:
      doc "The first character of the range (inclusive), as an int32 Unicode scalar value." $
      T.int32,
    "to">:
      doc "The last character of the range (inclusive), as an int32 Unicode scalar value." $
      T.int32]

classItem :: TypeDefinition
classItem = define "ClassItem" $
  doc "A member of a character class: either a single character or an inclusive character range." $
  T.union [
    "character">:
      doc "A single character (int32 Unicode scalar value) in the class." $
      T.int32,
    "range">:
      doc "An inclusive range of characters, e.g. a-z."
      characterRange]

quantified :: TypeDefinition
quantified = define "Quantified" $
  doc "An atom together with an optional quantifier applied to it." $
  T.record [
    "atom">:
      doc "The atom being quantified."
      atom,
    "quantifier">:
      doc "The quantifier; use 'one' for an unquantified atom."
      quantifier]

quantifier :: TypeDefinition
quantifier = define "Quantifier" $
  doc ("A regular-expression quantifier. Mirrors hydra.query.RegexQuantifier, which models the same"
    ++ " set of quantifiers for graph-path queries; a future refactor may hoist a shared type"
    ++ " (see #567 findings).") $
  T.union [
    "one">:
      doc "No quantifier; matches exactly one occurrence." T.unit,
    "zeroOrOne">:
      doc "The ? quantifier; matches zero or one occurrence." T.unit,
    "zeroOrMore">:
      doc "The * quantifier; matches zero or more occurrences." T.unit,
    "oneOrMore">:
      doc "The + quantifier; matches one or more occurrences." T.unit,
    "exactly">:
      doc "The {n} quantifier; matches exactly n occurrences." $
      T.int32,
    "atLeast">:
      doc "The {n,} quantifier; matches at least n occurrences." $
      T.int32,
    "range">:
      doc "The {n,m} quantifier; matches between n and m (inclusive) occurrences."
      quantifierRange]

quantifierRange :: TypeDefinition
quantifierRange = define "QuantifierRange" $
  doc "The bounds of a {n,m} quantifier: between min and max (inclusive) occurrences." $
  T.record [
    "min">:
      doc "The minimum number of occurrences (inclusive)." $
      T.int32,
    "max">:
      doc "The maximum number of occurrences (inclusive)." $
      T.int32]

regex :: TypeDefinition
regex = define "Regex" $
  doc ("A complete Hydra regular expression: the root of the AST. A regex is an alternation of"
    ++ " sequences.")
  alternation

sequence' :: TypeDefinition
sequence' = define "RegexSequence" $
  doc ("A (possibly empty) sequence of quantified atoms, matched in order. An empty sequence matches"
    ++ " the empty string. (Named RegexSequence rather than Sequence to avoid the generated coder"
    ++ " binding colliding with the host's built-in sequence.)") $
  T.list quantified
