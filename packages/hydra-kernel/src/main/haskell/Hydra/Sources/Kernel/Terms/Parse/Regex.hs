
module Hydra.Sources.Kernel.Terms.Parse.Regex where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (map)
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
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (apply, bind, char, map)
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
import           Prelude hiding ((++), pure, fail, map)
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Parsers as Parsers


ns :: ModuleName
ns = ModuleName "hydra.parse.regex"

-- Local Name constants for the hydra.regex types and fields. We do NOT import the generated
-- Hydra.Dsl.Regex wrapper: hydra.regex is a newly-added kernel type, so its DSL wrapper does not yet
-- exist when the driver compiles these Sources modules (bootstrap circular dependency). Following the
-- Read/Docs.hs precedent, we build the AST purely through Name constants + inject/record.
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

_Quantified :: Name
_Quantified = Name "hydra.regex.Quantified"
_Quantified_atom :: Name
_Quantified_atom = Name "atom"
_Quantified_quantifier :: Name
_Quantified_quantifier = Name "quantifier"

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

_QuantifierRange :: Name
_QuantifierRange = Name "hydra.regex.QuantifierRange"
_QuantifierRange_min :: Name
_QuantifierRange_min = Name "min"
_QuantifierRange_max :: Name
_QuantifierRange_max = Name "max"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Parsers.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just $
              "Parser for Hydra's translingual regular-expression syntax (docs/specification/regex.md):"
              <> " text -> hydra.regex AST. Built on the hydra.parsers combinators. Rejects ill-formed"
              <> " patterns (empty alternation branches, empty classes, out-of-range code points) via the"
              <> " ParseResult failure channel, so 'well-formed' is portable across hosts. See issue #567.")}
  where
   definitions = [
     toDefinition alternation,
     toDefinition atom,
     toDefinition characterClass,
     toDefinition classItem,
     toDefinition escapedChar,
     toDefinition isMetachar,
     toDefinition literalAtom,
     toDefinition parseRegex,
     toDefinition quantified,
     toDefinition quantifier,
     toDefinition regex,
     toDefinition sequence',
     toDefinition unsignedInt]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Codepoint constants for the metacharacters, to avoid host-specific char literals in the DSL.
cp :: Char -> TypedTerm Int
cp c = int32 (fromIntegral (fromEnum c))

-- | Is a codepoint a top-level metacharacter (. ^ $ * + ? ( ) [ ] { } | \)?
isMetachar :: TypedTermDefinition (Int -> Bool)
isMetachar = define "isMetachar" $
  doc "True if the codepoint is a top-level regex metacharacter that must be escaped to match literally." $
  "c" ~>
    Lists.foldl
      ("acc" ~> "m" ~> Logic.or (var "acc") (Equality.equal (var "c") (var "m")))
      false
      (list [
        cp '.', cp '^', cp '$', cp '*', cp '+', cp '?',
        cp '(', cp ')', cp '[', cp ']', cp '{', cp '}', cp '|', cp '\\'])

-- | Parse a backslash-escape: \\x yields the codepoint x (any character may follow a backslash).
escapedChar :: TypedTermDefinition (Parser Int)
escapedChar = define "escapedChar" $
  doc "Parse a backslash followed by any character; yields that character's codepoint (the escape is consumed)." $
  Parsers.bind @@ (Parsers.char @@ cp '\\') @@ (constant $
    Parsers.anyChar)

-- | Parse a single literal atom: an escaped metacharacter, or any non-metacharacter.
literalAtom :: TypedTermDefinition (Parser Term)
literalAtom = define "literalAtom" $
  doc "Parse a literal character (an escaped metacharacter, or any ordinary non-metacharacter) into an Atom." $
  Parsers.map @@ ("c" ~> inject _Atom _Atom_literal (var "c")) @@
    (Parsers.alt
      @@ escapedChar
      @@ (Parsers.satisfy @@ ("c" ~> Logic.not (isMetachar @@ var "c"))))

-- | Parse a class item: a range c-c or a single (possibly escaped) character.
classItem :: TypedTermDefinition (Parser Term)
classItem = define "classItem" $
  doc "Parse one member of a character class: a range like a-z, or a single character (escapes allowed)." $
  lets [
    -- inside a class, the escapable metaset is { \ ] ^ - }; but we accept a backslash-escape of any char
    "classChar">: Parsers.alt
      @@ escapedChar
      @@ (Parsers.satisfy @@ ("c" ~> Logic.and
            (Logic.not (Equality.equal (var "c") (cp ']')))
            (Logic.not (Equality.equal (var "c") (cp '\\')))))] $
    Parsers.bind @@ var "classChar" @@ ("from" ~>
      Parsers.alt
        -- range: '-' followed by another class char
        @@ (Parsers.bind @@ (Parsers.char @@ cp '-') @@ (constant $
              Parsers.map @@ ("to" ~> inject _ClassItem _ClassItem_range
                    (record _CharacterRange [
                      _CharacterRange_from>>: var "from",
                      _CharacterRange_to>>: var "to"]))
                @@ var "classChar"))
        -- single character
        @@ (Parsers.pure @@ inject _ClassItem _ClassItem_character (var "from")))

-- | Parse a bracketed character class [ ... ] or [^ ... ]. Empty classes are rejected (items are non-empty).
characterClass :: TypedTermDefinition (Parser Term)
characterClass = define "characterClass" $
  doc "Parse a character class [ ... ] or [^ ... ]; the class must be non-empty." $
  Parsers.bind @@ (Parsers.char @@ cp '[') @@ (constant $
    Parsers.bind @@ (Parsers.optional @@ (Parsers.char @@ cp '^')) @@ ("neg" ~>
      Parsers.bind @@ (Parsers.some @@ classItem) @@ ("items" ~>
        Parsers.bind @@ (Parsers.char @@ cp ']') @@ (constant $
          Parsers.pure @@ inject _Atom _Atom_class
            (record _CharacterClass [
              _CharacterClass_negated>>: Optionals.cases (var "neg") false (constant true),
              _CharacterClass_items>>: var "items"])))))

-- | Parse a non-negative decimal integer.
unsignedInt :: TypedTermDefinition (Parser Int)
unsignedInt = define "unsignedInt" $
  doc "Parse a non-negative decimal integer (one or more digits)." $
  Parsers.map
    @@ ("digits" ~> Lists.foldl
          ("acc" ~> "d" ~> Math.add (Math.mul (var "acc") (int32 10))
            (Math.sub (var "d") (cp '0')))
          (int32 0)
          (var "digits"))
    @@ (Parsers.some @@ (Parsers.satisfy @@ ("c" ~> Logic.and
          (Equality.gte (var "c") (cp '0'))
          (Equality.lte (var "c") (cp '9')))))

-- | Parse a quantifier: * + ? or a brace form {n} {n,} {n,m}. Returns the Quantifier (default 'one').
quantifier :: TypedTermDefinition (Parser Term)
quantifier = define "quantifier" $
  doc "Parse an optional quantifier following an atom; yields 'one' when no quantifier is present." $
  Parsers.alt
    @@ (Parsers.choice @@ list [
          Parsers.bind @@ (Parsers.char @@ cp '*') @@ (constant $ Parsers.pure @@ inject _Quantifier _Quantifier_zeroOrMore unit),
          Parsers.bind @@ (Parsers.char @@ cp '+') @@ (constant $ Parsers.pure @@ inject _Quantifier _Quantifier_oneOrMore unit),
          Parsers.bind @@ (Parsers.char @@ cp '?') @@ (constant $ Parsers.pure @@ inject _Quantifier _Quantifier_zeroOrOne unit),
          braceQuantifier])
    @@ (Parsers.pure @@ inject _Quantifier _Quantifier_one unit)
  where
    -- { n } | { n , } | { n , m }   (no {,m} form)
    braceQuantifier =
      Parsers.bind @@ (Parsers.char @@ cp '{') @@ (constant $
        Parsers.bind @@ (asTerm unsignedInt) @@ ("n" ~>
          Parsers.bind @@ (Parsers.optional @@ (Parsers.char @@ cp ',')) @@ ("comma" ~>
            Optionals.cases (var "comma")
              -- no comma: {n}
              (Parsers.bind @@ (Parsers.char @@ cp '}') @@ (constant $
                 Parsers.pure @@ inject _Quantifier _Quantifier_exactly (var "n")))
              -- comma present: {n,} or {n,m}
              (constant $
                Parsers.bind @@ (Parsers.optional @@ (asTerm unsignedInt)) @@ ("mm" ~>
                  Parsers.bind @@ (Parsers.char @@ cp '}') @@ (constant $
                    Optionals.cases (var "mm")
                      (Parsers.pure @@ inject _Quantifier _Quantifier_atLeast (var "n"))
                      ("m" ~> Parsers.pure @@ inject _Quantifier _Quantifier_range
                        (record _QuantifierRange [
                          _QuantifierRange_min>>: var "n",
                          _QuantifierRange_max>>: var "m"]))))))))

-- | Parse an atom: a group, class, ., anchor, or literal.
atom :: TypedTermDefinition (Parser Term)
atom = define "atom" $
  doc "Parse a single atom: a group ( ... ), a class [ ... ], ., an anchor ^ or $, or a literal." $
  Parsers.choice @@ list [
    -- group: ( alternation )   (empty group () is allowed; the alternation may be empty)
    Parsers.map @@ ("g" ~> inject _Atom _Atom_group (var "g")) @@ (Parsers.between
      @@ (Parsers.char @@ cp '(')
      @@ (Parsers.char @@ cp ')')
      @@ (Parsers.lazy @@ (constant $ asTerm alternation))),
    asTerm characterClass,
    Parsers.bind @@ (Parsers.char @@ cp '.') @@ (constant $ Parsers.pure @@ inject _Atom _Atom_any unit),
    Parsers.bind @@ (Parsers.char @@ cp '^') @@ (constant $ Parsers.pure @@ inject _Atom _Atom_anchorStart unit),
    Parsers.bind @@ (Parsers.char @@ cp '$') @@ (constant $ Parsers.pure @@ inject _Atom _Atom_anchorEnd unit),
    asTerm literalAtom]

-- | Parse an atom together with its optional quantifier.
quantified :: TypedTermDefinition (Parser Term)
quantified = define "quantified" $
  doc "Parse an atom followed by an optional quantifier." $
  Parsers.bind @@ (asTerm atom) @@ ("a" ~>
    Parsers.map @@ ("q" ~> record _Quantified [
      _Quantified_atom>>: var "a",
      _Quantified_quantifier>>: var "q"]) @@ (asTerm quantifier))

-- | Parse a sequence: zero or more quantified atoms.
sequence' :: TypedTermDefinition (Parser [Term])
sequence' = define "regexSequence" $
  doc ("Parse a sequence of quantified atoms (a single alternation branch). May be empty only as the"
    <> " whole pattern; as an alternation branch it is constrained to be non-empty by the alternation"
    <> " parser's use of sepBy1 plus a non-empty check.") $
  Parsers.many @@ (asTerm quantified)

-- | Parse an alternation: one or more sequences separated by |.
-- When there is more than one branch, every branch must be non-empty (so a|, |b, a||b, a||| and the
-- nested case (a|) are all rejected, at every nesting level). A single empty branch is allowed — it is
-- the legal empty case (the empty whole pattern, or an empty group ()).
alternation :: TypedTermDefinition (Parser [Term])
alternation = define "alternation" $
  doc ("Parse an alternation of sequences separated by |. When there is more than one branch, each"
    <> " branch must be non-empty; empty branches (a|, |b, a||b, and the nested (a|)) are rejected at"
    <> " every level. A single empty branch is the legal empty case (empty whole pattern / empty group).") $
  Parsers.bind @@ (Parsers.sepBy1 @@ (asTerm sequence') @@ (Parsers.char @@ cp '|')) @@ ("branches" ~>
    lets [
      "hasEmpty">: Lists.foldl
        ("acc" ~> "b" ~> Logic.or (var "acc") (Lists.null (var "b")))
        false
        (var "branches"),
      "multi">: Equality.gt (Lists.length (var "branches")) (int32 1)] $
    Logic.ifElse (Logic.and (var "hasEmpty") (var "multi"))
      (Parsers.fail @@ string "empty alternation branch")
      (Parsers.pure @@ var "branches"))

-- | The top-level regex parser: alternation, but the empty string is a legal whole pattern.
regex :: TypedTermDefinition (Parser [Term])
regex = define "regex" $
  doc "Parse a complete regex (an alternation). The empty pattern parses to a single empty sequence." $
  asTerm alternation

-- | Run the regex parser on a full input string, requiring all input to be consumed.
-- Returns nothing on failure (ill-formed pattern) or leftover input, per the portable-failure rule.
parseRegex :: TypedTermDefinition (String -> Maybe Term)
parseRegex = define "parseRegex" $
  doc ("Parse a full regex pattern string into a hydra.regex AST. Returns nothing if the pattern is"
    <> " ill-formed or does not consume all input; a well-formed pattern is exactly one that parses"
    <> " here, so 'well-formed' is portable across all hosts.") $
  "input" ~>
    -- Empty-branch rejection is handled inside 'alternation'; here we only require that a successful
    -- parse consumed ALL input (no trailing garbage).
    cases _ParseResult (Parsers.runParser @@ (asTerm regex) @@ var "input") Nothing [
      _ParseResult_success>>: "s" ~>
        Logic.ifElse (Equality.equal (Parsing.parseSuccessRemainder $ var "s") (string ""))
          (just (Parsing.parseSuccessValue (var "s")))
          nothing,
      _ParseResult_failure>>: "e" ~> nothing]
