
module Hydra.Sources.Kernel.Terms.Print.Posix.Regex where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.print.posix.regex"

-- Local Name constants for the hydra.core.regex types and fields (the generated Hydra.Core.Dsl.Regex wrapper is
-- not available at Sources-compile time; see Print/Regex.hs). Referenced via project/match only.
_CharacterRange :: Name
_CharacterRange = Name "hydra.core.regex.CharacterRange"
_CharacterRange_from :: Name
_CharacterRange_from = Name "from"
_CharacterRange_to :: Name
_CharacterRange_to = Name "to"

_CharacterClass :: Name
_CharacterClass = Name "hydra.core.regex.CharacterClass"
_CharacterClass_negated :: Name
_CharacterClass_negated = Name "negated"
_CharacterClass_items :: Name
_CharacterClass_items = Name "items"

_QuantifierRange :: Name
_QuantifierRange = Name "hydra.core.regex.QuantifierRange"
_QuantifierRange_min :: Name
_QuantifierRange_min = Name "min"
_QuantifierRange_max :: Name
_QuantifierRange_max = Name "max"

_Quantified :: Name
_Quantified = Name "hydra.core.regex.Quantified"
_Quantified_atom :: Name
_Quantified_atom = Name "atom"
_Quantified_quantifier :: Name
_Quantified_quantifier = Name "quantifier"

_Atom :: Name
_Atom = Name "hydra.core.regex.Atom"
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
_ClassItem = Name "hydra.core.regex.ClassItem"
_ClassItem_character :: Name
_ClassItem_character = Name "character"
_ClassItem_range :: Name
_ClassItem_range = Name "range"

_Quantifier :: Name
_Quantifier = Name "hydra.core.regex.Quantifier"
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
              "Per-dialect printer rendering the hydra.core.regex AST into POSIX ERE syntax (Haskell"
              <> " Text.Regex.TDFA, Scheme Guile (ice-9 regex)). Differs from the canonical"
              <> " hydra.core.print.regex in three places, each probe-verified against TDFA (docs/"
              <> " specification/regex.md and issue #567/#603): (1) Hydra's . (any character INCLUDING"
              <> " newline) renders as the explicit full-code-point-range class"
              <> " [<U+0000>-<U+10FFFF>], because POSIX ERE's native . excludes newline and does not"
              <> " support the \\s / \\S shorthands; (2) anchors ^ and $ (whole-STRING boundaries in"
              <> " Hydra) render as the GNU/glibc whole-buffer anchors \\` and \\', because TDFA's"
              <> " native ^ and $ are line-oriented; (3) character-class metacharacters ] ^ - are"
              <> " disambiguated POSITIONALLY (] first, - last, ^ never first) rather than by"
              <> " backslash-escaping, because POSIX ERE bracket expressions have no escape mechanism"
              <> " at all (a backslash inside [...] is an ordinary character on TDFA). All other"
              <> " productions are native POSIX ERE.")}
  where
   definitions = [
     toDefinition alternation,
     toDefinition anyClass,
     toDefinition atom,
     toDefinition characterClass,
     toDefinition classItem,
     toDefinition classItemIsLiteral,
     toDefinition escapeLiteral,
     toDefinition printCodePoint,
     toDefinition printRegex,
     toDefinition quantified,
     toDefinition quantifier,
     toDefinition sequence']

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

cp :: Char -> TypedTerm Int
cp c = int32 (fromIntegral (fromEnum c))

printCodePoint :: TypedTermDefinition (Int -> String)
printCodePoint = define "printCodePoint" $
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
    printCodePoint @@ int32 0,
    string "-",
    printCodePoint @@ int32 1114111,
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
      (Strings.concat2 (string "\\") (printCodePoint @@ var "c"))
      (printCodePoint @@ var "c")

-- | True if the item is exactly the single literal character code point c (not a range).
classItemIsLiteral :: TypedTermDefinition (Int -> Term -> Bool)
classItemIsLiteral = define "classItemIsLiteral" $
  doc "True if a ClassItem is the single literal character code point c (not a range)." $
  "c" ~> "item" ~>
    match _ClassItem (var "item") (Just false) [
      _ClassItem_character>>: "ic" ~> Equality.equal (var "ic") (var "c")]

classItem :: TypedTermDefinition (Term -> String)
classItem = define "classItem" $
  doc ("Render one character-class member (a single character or an inclusive range). POSIX ERE"
    <> " bracket expressions do NOT support backslash escaping (confirmed by TDFA probe: \\- and \\]"
    <> " inside [...] are two literal characters, not an escape); the metacharacters ] ^ - are instead"
    <> " disambiguated positionally by characterClass (] first, - last, ^ never first), so a plain"
    <> " character or range here never needs an escape.") $
  "item" ~>
    match _ClassItem (var "item") Nothing [
      _ClassItem_character>>: "c" ~> printCodePoint @@ var "c",
      _ClassItem_range>>: "r" ~> Strings.concat $ list [
        printCodePoint @@ (project _CharacterRange _CharacterRange_from @@ var "r"),
        string "-",
        printCodePoint @@ (project _CharacterRange _CharacterRange_to @@ var "r")]]

characterClass :: TypedTermDefinition (Term -> String)
characterClass = define "characterClass" $
  doc ("Render a character class, including the leading ^ for a negated class. Reorders items"
    <> " positionally for POSIX ERE (which has no bracket-expression escaping): a literal ] is moved"
    <> " to the front (immediately after the optional negating ^), a literal - is moved to the back,"
    <> " and (only when there is no literal ] item to occupy the first slot) a literal ^ is moved"
    <> " after the first non-^ item -- POSIX treats a leading ^ as the negation marker regardless of"
    <> " Hydra's negated flag. The one residual gap: a class whose only member is a literal ^ has no"
    <> " safe position on POSIX ERE; this is an extreme corner case (single-item classes are otherwise"
    <> " unremarkable) and is left unhandled here.") $
  "cc" ~>
    lets [
      "items">: project _CharacterClass _CharacterClass_items @@ var "cc",
      "bracketAndRest">: Lists.partition (classItemIsLiteral @@ cp ']') (var "items"),
      "bracketItems">: Pairs.first (var "bracketAndRest"),
      "afterBracket">: Pairs.second (var "bracketAndRest"),
      "dashAndRest">: Lists.partition (classItemIsLiteral @@ cp '-') (var "afterBracket"),
      "dashItems">: Pairs.first (var "dashAndRest"),
      "rest">: Pairs.second (var "dashAndRest"),
      "caretAndNonCaret">: Lists.partition (classItemIsLiteral @@ cp '^') (var "rest"),
      "caretItems">: Pairs.first (var "caretAndNonCaret"),
      "nonCaretItems">: Pairs.second (var "caretAndNonCaret"),
      -- If a literal ] already occupies the first slot, ^ is safe anywhere in 'rest' (original
      -- order is fine). Otherwise, any literal ^ must be moved after at least one non-^ item.
      "safeRest">: Logic.ifElse
        (Logic.or (Logic.not (Lists.isEmpty (var "bracketItems"))) (Lists.isEmpty (var "nonCaretItems")))
        (var "rest")
        (Lists.concat2 (var "nonCaretItems") (var "caretItems")),
      "ordered">: Lists.concat (list [var "bracketItems", var "safeRest", var "dashItems"])] $
    Strings.concat $ list [
      string "[",
      Logic.ifElse (project _CharacterClass _CharacterClass_negated @@ var "cc") (string "^") (string ""),
      Strings.concat (Lists.map (asTerm classItem) (var "ordered")),
      string "]"]

-- The dialect divergence: Hydra's . (any incl. newline) renders as the explicit full-range class
-- (anyClass), NOT a bare . (POSIX . excludes newline) and NOT [\s\S] (unsupported on TDFA).
atom :: TypedTermDefinition (Term -> String)
atom = define "atom" $
  doc "Render a single atom; . renders as the explicit full-code-point-range class (POSIX-safe)." $
  "a" ~>
    match _Atom (var "a") Nothing [
      _Atom_literal>>: "c" ~> escapeLiteral @@ var "c",
      _Atom_any>>: constant (asTerm anyClass),
      -- Hydra anchors are whole-STRING boundaries (docs/specification/regex.md), but TDFA's native ^
      -- and $ are line-oriented (probe-confirmed: ^b$ matches "b" inside "a\nb\nc"). The GNU/glibc
      -- whole-buffer anchors \` and \' give the correct whole-string semantics on TDFA (probe-
      -- confirmed: \`b\' does NOT match "b" inside "a\nb\nc").
      _Atom_anchorStart>>: constant (string "\\`"),
      _Atom_anchorEnd>>: constant (string "\\'"),
      _Atom_group>>: "g" ~> Strings.concat $ list [
        string "(",
        alternation @@ var "g",
        string ")"],
      _Atom_class>>: "cc" ~> characterClass @@ var "cc"]

quantifier :: TypedTermDefinition (Term -> String)
quantifier = define "quantifier" $
  doc "Render a quantifier suffix; the 'one' quantifier renders as the empty string." $
  "q" ~>
    match _Quantifier (var "q") Nothing [
      _Quantifier_one>>: constant (string ""),
      _Quantifier_zeroOrOne>>: constant (string "?"),
      _Quantifier_zeroOrMore>>: constant (string "*"),
      _Quantifier_oneOrMore>>: constant (string "+"),
      _Quantifier_exactly>>: "n" ~> Strings.concat $ list [
        string "{", Literals.printInt32 (var "n"), string "}"],
      _Quantifier_atLeast>>: "n" ~> Strings.concat $ list [
        string "{", Literals.printInt32 (var "n"), string ",}"],
      _Quantifier_range>>: "r" ~> Strings.concat $ list [
        string "{",
        Literals.printInt32 (project _QuantifierRange _QuantifierRange_min @@ var "r"),
        string ",",
        Literals.printInt32 (project _QuantifierRange _QuantifierRange_max @@ var "r"),
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
  doc ("Render a hydra.core.regex AST into POSIX ERE syntax. Identical to the canonical printer except that"
    <> " . (any incl. newline) becomes the explicit full-code-point-range class.") $
  "r" ~> alternation @@ var "r"
