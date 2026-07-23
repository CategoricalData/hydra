
module Hydra.Sources.Kernel.Terms.Print.Posix.Regex where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.print.posix.regex"

-- Local Name constants for the hydra.regex types and fields (the generated Hydra.Dsl.Regex wrapper is
-- not available at Sources-compile time; see Print/Regex.hs). Referenced via project/cases only.
_CharacterRange :: Name
_CharacterRange = Name "hydra.regex.CharacterRange"
_CharacterRange_from :: Name
_CharacterRange_from = Name "from"
_CharacterRange_to :: Name
_CharacterRange_to = Name "to"

_CharacterClass :: Name
_CharacterClass = Name "hydra.regex.CharacterClass"
_CharacterClass_negated :: Name
_CharacterClass_negated = Name "negated"
_CharacterClass_items :: Name
_CharacterClass_items = Name "items"

_QuantifierRange :: Name
_QuantifierRange = Name "hydra.regex.QuantifierRange"
_QuantifierRange_min :: Name
_QuantifierRange_min = Name "min"
_QuantifierRange_max :: Name
_QuantifierRange_max = Name "max"

_Quantified :: Name
_Quantified = Name "hydra.regex.Quantified"
_Quantified_atom :: Name
_Quantified_atom = Name "atom"
_Quantified_quantifier :: Name
_Quantified_quantifier = Name "quantifier"

_Atom :: Name
_Atom = Name "hydra.regex.Atom"
_Atom_literal :: Name
_Atom_literal = Name "literal"
_Atom_any :: Name
_Atom_any = Name "any"
_Atom_anchorStart :: Name
_Atom_anchorStart = Name "anchorStart"
_Atom_anchorEnd :: Name
_Atom_anchorEnd = Name "anchorEnd"
_Atom_group :: Name
_Atom_group = Name "group"
_Atom_class :: Name
_Atom_class = Name "class"

_ClassItem :: Name
_ClassItem = Name "hydra.regex.ClassItem"
_ClassItem_character :: Name
_ClassItem_character = Name "character"
_ClassItem_range :: Name
_ClassItem_range = Name "range"

_Quantifier :: Name
_Quantifier = Name "hydra.regex.Quantifier"
_Quantifier_one :: Name
_Quantifier_one = Name "one"
_Quantifier_zeroOrOne :: Name
_Quantifier_zeroOrOne = Name "zeroOrOne"
_Quantifier_zeroOrMore :: Name
_Quantifier_zeroOrMore = Name "zeroOrMore"
_Quantifier_oneOrMore :: Name
_Quantifier_oneOrMore = Name "oneOrMore"
_Quantifier_exactly :: Name
_Quantifier_exactly = Name "exactly"
_Quantifier_atLeast :: Name
_Quantifier_atLeast = Name "atLeast"
_Quantifier_range :: Name
_Quantifier_range = Name "range"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just $
              "Per-dialect printer rendering the hydra.regex AST into POSIX ERE syntax (Haskell"
              <> " Text.Regex.TDFA, Scheme Guile (ice-9 regex)). Differs from the canonical"
              <> " hydra.print.regex in one place: Hydra's . (any character INCLUDING newline) renders as"
              <> " the explicit full-code-point-range class [<U+0000>-<U+10FFFF>], because POSIX ERE's"
              <> " native . excludes newline and POSIX ERE does not support the \\s / \\S shorthands"
              <> " (confirmed by TDFA probe). All other productions are native POSIX ERE. See"
              <> " docs/specification/regex.md and issue #567.")}
  where
   definitions = [
     toDefinition alternation,
     toDefinition anyClass,
     toDefinition atom,
     toDefinition characterClass,
     toDefinition classItem,
     toDefinition escapeClassChar,
     toDefinition escapeLiteral,
     toDefinition printRegex,
     toDefinition quantified,
     toDefinition quantifier,
     toDefinition sequence',
     toDefinition showCodePoint]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

cp :: Char -> TypedTerm Int
cp c = int32 (fromIntegral (fromEnum c))

showCodePoint :: TypedTermDefinition (Int -> String)
showCodePoint = define "showCodePoint" $
  doc "Render a single Unicode code point as a one-character string." $
  "c" ~> Strings.fromList (list [var "c"])

-- The full-code-point-range class that stands in for Hydra's newline-inclusive '.' under POSIX ERE.
-- TDFA accepts an explicit literal-character range spanning [U+0000, U+10FFFF] (probe-verified), which
-- matches every character including newline. 1114111 = 0x10FFFF.
anyClass :: TypedTermDefinition String
anyClass = define "anyClass" $
  doc ("The POSIX ERE rendering of Hydra's . (any character incl. newline): an explicit character-class"
    <> " range spanning the whole Unicode scalar range, which POSIX ERE engines match literally.") $
  Strings.concat $ list [
    string "[",
    showCodePoint @@ int32 0,
    string "-",
    showCodePoint @@ int32 1114111,
    string "]"]

escapeLiteral :: TypedTermDefinition (Int -> String)
escapeLiteral = define "escapeLiteral" $
  doc "Render a literal code point in top-level context, backslash-escaping POSIX ERE metacharacters." $
  "c" ~>
    lets [
      "isMeta">: Lists.foldl
        ("acc" ~> "m" ~> Logic.or (var "acc") (Equality.equal (var "c") (var "m")))
        false
        (list [
          cp '.', cp '^', cp '$', cp '*', cp '+', cp '?',
          cp '(', cp ')', cp '[', cp ']', cp '{', cp '}', cp '|', cp '\\'])] $
    Logic.ifElse (var "isMeta")
      (Strings.concat2 (string "\\") (showCodePoint @@ var "c"))
      (showCodePoint @@ var "c")

escapeClassChar :: TypedTermDefinition (Int -> String)
escapeClassChar = define "escapeClassChar" $
  doc "Render a code point inside a character class, uniformly escaping the class metacharacters \\ ] ^ -." $
  "c" ~>
    lets [
      "isMeta">: Lists.foldl
        ("acc" ~> "m" ~> Logic.or (var "acc") (Equality.equal (var "c") (var "m")))
        false
        (list [cp '\\', cp ']', cp '^', cp '-'])] $
    Logic.ifElse (var "isMeta")
      (Strings.concat2 (string "\\") (showCodePoint @@ var "c"))
      (showCodePoint @@ var "c")

classItem :: TypedTermDefinition (Term -> String)
classItem = define "classItem" $
  doc "Render one character-class member (a single character or an inclusive range)." $
  "item" ~>
    cases _ClassItem (var "item") Nothing [
      _ClassItem_character>>: "c" ~> escapeClassChar @@ var "c",
      _ClassItem_range>>: "r" ~> Strings.concat $ list [
        escapeClassChar @@ (project _CharacterRange _CharacterRange_from @@ var "r"),
        string "-",
        escapeClassChar @@ (project _CharacterRange _CharacterRange_to @@ var "r")]]

characterClass :: TypedTermDefinition (Term -> String)
characterClass = define "characterClass" $
  doc "Render a character class, including the leading ^ for a negated class." $
  "cc" ~>
    Strings.concat $ list [
      string "[",
      Logic.ifElse (project _CharacterClass _CharacterClass_negated @@ var "cc") (string "^") (string ""),
      Strings.concat (Lists.map (asTerm classItem) (project _CharacterClass _CharacterClass_items @@ var "cc")),
      string "]"]

-- The dialect divergence: Hydra's . (any incl. newline) renders as the explicit full-range class
-- (anyClass), NOT a bare . (POSIX . excludes newline) and NOT [\s\S] (unsupported on TDFA).
atom :: TypedTermDefinition (Term -> String)
atom = define "atom" $
  doc "Render a single atom; . renders as the explicit full-code-point-range class (POSIX-safe)." $
  "a" ~>
    cases _Atom (var "a") Nothing [
      _Atom_literal>>: "c" ~> escapeLiteral @@ var "c",
      _Atom_any>>: constant (asTerm anyClass),
      _Atom_anchorStart>>: constant (string "^"),
      _Atom_anchorEnd>>: constant (string "$"),
      _Atom_group>>: "g" ~> Strings.concat $ list [
        string "(",
        alternation @@ var "g",
        string ")"],
      _Atom_class>>: "cc" ~> characterClass @@ var "cc"]

quantifier :: TypedTermDefinition (Term -> String)
quantifier = define "quantifier" $
  doc "Render a quantifier suffix; the 'one' quantifier renders as the empty string." $
  "q" ~>
    cases _Quantifier (var "q") Nothing [
      _Quantifier_one>>: constant (string ""),
      _Quantifier_zeroOrOne>>: constant (string "?"),
      _Quantifier_zeroOrMore>>: constant (string "*"),
      _Quantifier_oneOrMore>>: constant (string "+"),
      _Quantifier_exactly>>: "n" ~> Strings.concat $ list [
        string "{", Literals.showInt32 (var "n"), string "}"],
      _Quantifier_atLeast>>: "n" ~> Strings.concat $ list [
        string "{", Literals.showInt32 (var "n"), string ",}"],
      _Quantifier_range>>: "r" ~> Strings.concat $ list [
        string "{",
        Literals.showInt32 (project _QuantifierRange _QuantifierRange_min @@ var "r"),
        string ",",
        Literals.showInt32 (project _QuantifierRange _QuantifierRange_max @@ var "r"),
        string "}"]]

quantified :: TypedTermDefinition (Term -> String)
quantified = define "quantified" $
  doc "Render an atom followed by its quantifier suffix." $
  "qa" ~>
    Strings.concat2
      (atom @@ (project _Quantified _Quantified_atom @@ var "qa"))
      (quantifier @@ (project _Quantified _Quantified_quantifier @@ var "qa"))

sequence' :: TypedTermDefinition ([Term] -> String)
sequence' = define "regexSequence" $
  doc "Render a sequence of quantified atoms by concatenation." $
  "s" ~> Strings.concat (Lists.map (asTerm quantified) (var "s"))

alternation :: TypedTermDefinition ([Term] -> String)
alternation = define "alternation" $
  doc "Render an alternation, joining its branches with the | operator." $
  "alt" ~> Strings.join (string "|") (Lists.map (asTerm sequence') (var "alt"))

printRegex :: TypedTermDefinition ([Term] -> String)
printRegex = define "printRegex" $
  doc ("Render a hydra.regex AST into POSIX ERE syntax. Identical to the canonical printer except that"
    <> " . (any incl. newline) becomes the explicit full-code-point-range class.") $
  "r" ~> alternation @@ var "r"
