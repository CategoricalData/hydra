{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Parse.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType, map)
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
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
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
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (
  apply, bind, binding, char, field, fieldType, floatType, floatValue, injection, integerType,
  integerValue, lambda, literal, literalType, map, project, term, type_, typeScheme)
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

import qualified Hydra.Sources.Kernel.Terms.Parsers as P
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore


-- | #497 W2: a combinator parser for docs/specification/syntax.md's textual notation for terms,
-- types, literals, and type schemes -- the counterpart to PrintCore's term/type_/typeScheme (W4,
-- landed). See feature_497_print_parse_syntax's plan doc (section "W2 -- hydra.core.parse.core
-- parser design") for the full design this mirrors: precedence-climbing shape, combinator-fit
-- analysis (no 'try'/backtracking needed -- every term-starting keyword is followed by a
-- delimiter a bare name segment can't contain, so this is LL(1)-by-construction), and the
-- set-vs-map one-token-lookahead disambiguation. Output contract: produces plain hydra.core
-- Terms/Types with annotations attached exactly as parsed (round-trip contract, syntax.md #1) --
-- no inference, no name resolution.
--
-- NOT implemented here: the kinded binder form (reserved for #226, not yet emitted by any
-- printer, so not accepted here either -- accepting it now would make this parser accept
-- strictly more than the printer ever emits, risking silent drift once #226 lands and defines
-- real semantics for it).

ns :: ModuleName
ns = ModuleName "hydra.core.parse.core"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([P.ns, PrintCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              "Parsers for hydra.core.model types, per docs/specification/syntax.md. The counterpart to hydra.core.print.model.")}
  where
   definitions = [
     toDefinition annotSuffix,
     toDefinition atomicTerm,
     toDefinition backtickedName,
     toDefinition bareNameSegmentChar,
     toDefinition binderName,
     toDefinition binding,
     toDefinition braceContent,
     toDefinition caseTerm,
     toDefinition constraint,
     toDefinition decimalDigits,
     toDefinition eitherTerm,
     toDefinition escapeHex4,
     toDefinition escapeHexDigit,
     toDefinition escapeHexDigitValue,
     toDefinition escapedChar,
     toDefinition floatSuffix,
     toDefinition injectTerm,
     toDefinition integerSuffix,
     toDefinition lambdaTerm,
     toDefinition letTerm,
     toDefinition lexeme,
     toDefinition listTerm,
     toDefinition literalLit,
     toDefinition literalTypeLit,
     toDefinition name,
     toDefinition numericLit,
     toDefinition optionalTerm,
     toDefinition parseTerm,
     toDefinition parseTermSignature,
     toDefinition parseType,
     toDefinition parseTypeScheme,
     toDefinition postfixTerm,
     toDefinition postfixType,
     toDefinition projectTerm,
     toDefinition recordTerm,
     toDefinition signedDigits,
     toDefinition stringLit,
     toDefinition term,
     toDefinition termField,
     toDefinition termFields,
     toDefinition termKeyword,
     toDefinition termSignature,
     toDefinition termSignatureArrowChain,
     toDefinition topTerm,
     toDefinition topType,
     toDefinition type_,
     toDefinition typeApplicationSuffix,
     toDefinition typeAtom,
     toDefinition typeFunctionLevel,
     toDefinition typeKeyword,
     toDefinition typeScheme,
     toDefinition unsignedDigits,
     toDefinition unwrapTerm,
     toDefinition whitespace,
     toDefinition wrapTerm]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Skip zero or more whitespace characters, including '#' line comments (syntax.md #2.2).
whitespace :: TypedTermDefinition (Parser ())
whitespace = define "whitespace" $
  doc "Skip zero or more whitespace characters and '#' line comments" $
  P.map @@ ("_" ~> unit) @@ (P.many @@ (P.alt @@ wsChar @@ lineComment))
  where
    wsChar = P.map @@ (constant unit) @@ (P.satisfy @@ ("c" ~> Chars.isSpace (var "c")))
    lineComment =
      P.bind @@ (P.char @@ int32 35) @@ (constant $    -- '#'
        P.map @@ (constant unit) @@
          (P.many @@ (P.satisfy @@ ("c" ~> Logic.not (Equality.equal (var "c") (int32 10))))))  -- not '\n'

-- | Parse a token followed by trailing whitespace/comments (the 'lexeme' convention: whitespace
-- is consumed AFTER a token, never before one, so that 'alt'/'optional' can still backtrack when
-- the failed alternative consumed no real input -- see Parse/Packaging.hs's 'token' for the
-- original statement of this rule, which this mirrors under a different name since 'token' here
-- would collide with this module's own atomic-term-vs-keyword terminology).
lexeme :: TypedTermDefinition (Parser a -> Parser a)
lexeme = define "lexeme" $
  doc "Parse a token followed by optional trailing whitespace/comments" $
  "p" ~> P.bind @@ var "p" @@ ("x" ~> P.bind @@ whitespace @@ (constant $ P.pure @@ var "x"))

-- | Parse a specific keyword as a lexeme.
termKeyword :: TypedTermDefinition (String -> Parser String)
termKeyword = define "termKeyword" $
  doc "Parse a specific keyword, followed by optional trailing whitespace/comments" $
  "kw" ~> lexeme @@ (P.string_ @@ var "kw")

-- | Alias of 'termKeyword' for use in type-grammar productions; same parser, named for
-- readability at type-parser call sites.
typeKeyword :: TypedTermDefinition (String -> Parser String)
typeKeyword = define "typeKeyword" $
  doc "Parse a specific keyword, followed by optional trailing whitespace/comments (type-grammar alias of termKeyword)" $
  "kw" ~> termKeyword @@ var "kw"

-- | True iff a codepoint is a bare-name-segment character: alphanumeric, per syntax.md #2.4.
bareNameSegmentChar :: TypedTermDefinition (Int -> Bool)
bareNameSegmentChar = define "bareNameSegmentChar" $
  doc "True iff the codepoint may appear in a bare name segment (alphanumeric, per syntax.md #2.4)" $
  "c" ~> Chars.isAlphaNum (var "c")

-- | Compute the numeric value (0-15) of a hex digit character code, or Nothing if not one.
-- Mirrors hydra.core.json.parser.jsonHexDigitValue (not imported directly: hydra.core.json.parser
-- itself depends on Hydra.Sources.Kernel.Terms.All, so depending on it here would create a
-- module-graph cycle once this module is registered in All.hs's own module list). Per syntax.md
-- #2.5's "the same kernel code SHOULD serve JSON and textual printing and parsing of strings":
-- this follows the same RULES as the JSON reader, independently implemented to avoid the cycle.
escapeHexDigitValue :: TypedTermDefinition (Int -> Maybe Int)
escapeHexDigitValue = define "escapeHexDigitValue" $
  doc "Compute the numeric value of a hex digit character code (0-15), or Nothing if not a hex digit" $
  "c" ~>
    Logic.ifElse
      (Logic.and (Ordering.gte (var "c") (int32 48)) (Ordering.lte (var "c") (int32 57)))    -- '0'-'9'
      (just $ Math.sub (var "c") (int32 48))
      (Logic.ifElse
        (Logic.and (Ordering.gte (var "c") (int32 97)) (Ordering.lte (var "c") (int32 102)))    -- 'a'-'f'
        (just $ Math.add (int32 10) (Math.sub (var "c") (int32 97)))
        (Logic.ifElse
          (Logic.and (Ordering.gte (var "c") (int32 65)) (Ordering.lte (var "c") (int32 70)))    -- 'A'-'F'
          (just $ Math.add (int32 10) (Math.sub (var "c") (int32 65)))
          nothing))

-- | Parse a single hex digit, yielding its numeric value (0-15).
escapeHexDigit :: TypedTermDefinition (Parser Int)
escapeHexDigit = define "escapeHexDigit" $
  doc "Parse a single hex digit character, yielding its numeric value (0-15)" $
  P.bind @@ P.anyChar @@ ("c" ~>
    Optionals.match (escapeHexDigitValue @@ var "c")
      (P.fail @@ string "expected a hex digit")
      ("v" ~> P.pure @@ var "v"))

-- | Parse exactly four hex digits, yielding the 16-bit code unit they encode.
escapeHex4 :: TypedTermDefinition (Parser Int)
escapeHex4 = define "escapeHex4" $
  doc "Parse exactly four hex digits as a 16-bit code unit" $
  P.bind @@ escapeHexDigit @@ ("d1" ~>
    P.bind @@ escapeHexDigit @@ ("d2" ~>
      P.bind @@ escapeHexDigit @@ ("d3" ~>
        P.bind @@ escapeHexDigit @@ ("d4" ~>
          P.pure @@
            (Math.add (Math.mul (var "d1") (int32 4096))
              (Math.add (Math.mul (var "d2") (int32 256))
                (Math.add (Math.mul (var "d3") (int32 16)) (var "d4"))))))))

-- | Parse a string-escape sequence after the backslash (syntax.md #2.5: the JSON string-escape
-- rules), combining UTF-16 surrogate pairs into one supplementary-plane code point. Shared by
-- 'stringLit' (double-quote-delimited) and 'backtickedName' (backtick-delimited) -- both use the
-- same escape set, differing only in which character terminates the literal.
escapedChar :: TypedTermDefinition (Parser Int)
escapedChar = define "escapedChar" $
  doc "Parse a string-escape sequence after the backslash, combining surrogate pairs" $
  P.bind @@ (P.char @@ int32 92) @@ (constant $    -- '\\'
    P.bind @@ escapeChar1 @@ ("u1" ~>
      Logic.ifElse (isHighSurrogate @@ var "u1")
        (P.bind @@ (P.char @@ int32 92) @@ (constant $    -- '\\'
          P.bind @@ (P.char @@ int32 117) @@ (constant $    -- 'u'
            P.bind @@ escapeHex4 @@ ("u2" ~>
              Logic.ifElse (isLowSurrogate @@ var "u2")
                (P.pure @@
                  (Math.add (int32 0x10000)
                    (Math.add (Math.mul (Math.sub (var "u1") (int32 0xD800)) (int32 0x400))
                      (Math.sub (var "u2") (int32 0xDC00)))))
                (P.fail @@ string "invalid low surrogate in escaped surrogate pair")))))
        (Logic.ifElse (isLowSurrogate @@ var "u1")
          (P.fail @@ string "lone low surrogate in escape sequence")
          (P.pure @@ var "u1"))))
  where
    isHighSurrogate = "c" ~> Logic.and (Ordering.gte (var "c") (int32 0xD800)) (Ordering.lte (var "c") (int32 0xDBFF))
    isLowSurrogate  = "c" ~> Logic.and (Ordering.gte (var "c") (int32 0xDC00)) (Ordering.lte (var "c") (int32 0xDFFF))
    escapeChar1 = P.choice @@ list [
      P.map @@ (constant $ int32 34) @@ (P.char @@ int32 34),    -- '"'
      P.map @@ (constant $ int32 92) @@ (P.char @@ int32 92),    -- '\\'
      P.map @@ (constant $ int32 47) @@ (P.char @@ int32 47),    -- '/'
      P.map @@ (constant $ int32 8)  @@ (P.char @@ int32 98),    -- 'b' -> backspace
      P.map @@ (constant $ int32 12) @@ (P.char @@ int32 102),   -- 'f' -> form feed
      P.map @@ (constant $ int32 10) @@ (P.char @@ int32 110),   -- 'n' -> line feed
      P.map @@ (constant $ int32 13) @@ (P.char @@ int32 114),   -- 'r' -> carriage return
      P.map @@ (constant $ int32 9)  @@ (P.char @@ int32 116),   -- 't' -> tab
      P.bind @@ (P.char @@ int32 117) @@ (constant escapeHex4)]  -- 'u' -> \\uXXXX

-- | Parse the content of a backtick-escaped name (between the backticks), per syntax.md #2.5's
-- string-escape rules (shared with 'stringLit' via 'escapedChar') applied to the backtick
-- delimiter in place of the double-quote delimiter.
backtickedName :: TypedTermDefinition (Parser String)
backtickedName = define "backtickedName" $
  doc "Parse a backtick-escaped name, applying the string-escape rules to its content" $
  lexeme @@ (
    P.bind @@ (P.char @@ int32 96) @@ (constant $    -- '`'
      P.bind @@ (P.many @@ backtickChar) @@ ("cs" ~>
        P.bind @@ (P.char @@ int32 96) @@ (constant $
          P.pure @@ (Strings.fromList $ var "cs")))))
  where
    backtickChar = P.alt
      @@ asTerm escapedChar
      @@ (P.satisfy @@ ("c" ~> Logic.and
            (Logic.not (Equality.equal (var "c") (int32 96)))     -- not '`'
            (Logic.not (Equality.equal (var "c") (int32 92)))))   -- not '\\'

-- | Parse a name in an ordinary (non-binder-head) position: a bare dotted name, read greedily
-- (syntax.md #2.4: "outside binder heads, bare dotted names are read greedily as qualified
-- names"), or a backtick-escaped name. Reserved words are rejected in bare form (case 2 of
-- #2.4's interference rule) -- a reserved word can only be read here via its backtick form.
name :: TypedTermDefinition (Parser Name)
name = define "name" $
  doc "Parse a name: a greedily-read bare dotted name, or a backtick-escaped name" $
  P.map @@ ("s" ~> Core.name (var "s")) @@ (P.alt @@ bareDottedName @@ backtickedName)
  where
    bareDottedName = lexeme @@ (
      P.bind @@ (P.sepBy1 @@ segment @@ (P.char @@ int32 46)) @@ ("segs" ~>    -- '.'
        "s" <~ Strings.join (string ".") (var "segs") $
        Logic.ifElse (PrintCore.isReservedWord @@ var "s")
          (P.fail @@ (Strings.concat2 (string "reserved word used bare: ") (var "s")))
          (P.pure @@ var "s")))
    segment = P.map @@ ("cs" ~> Strings.fromList (var "cs")) @@
      (P.some @@ (P.satisfy @@ (asTerm bareNameSegmentChar)))

-- | Parse a name in a binder head (syntax.md #2.4 case 3): here a dot is structural (it
-- terminates the binder head), so a bare binder name must be a single dot-free segment --
-- UNLIKE 'name' above, which reads dotted names greedily. A reserved word is rejected in bare
-- form here too, for the same reason as 'name'.
binderName :: TypedTermDefinition (Parser Name)
binderName = define "binderName" $
  doc "Parse a binder-head name: a single dot-free bare segment, or a backtick-escaped name" $
  P.map @@ ("s" ~> Core.name (var "s")) @@ (P.alt @@ bareSegment @@ backtickedName)
  where
    bareSegment = lexeme @@ (
      P.bind @@ (P.map @@ ("cs" ~> Strings.fromList (var "cs")) @@
        (P.some @@ (P.satisfy @@ (asTerm bareNameSegmentChar)))) @@ ("s" ~>
        Logic.ifElse (PrintCore.isReservedWord @@ var "s")
          (P.fail @@ (Strings.concat2 (string "reserved word used bare: ") (var "s")))
          (P.pure @@ var "s")))

-- Codepoint helpers for digit runs (shared by integer/float/decimal literal parsing).
unsignedDigits :: TypedTermDefinition (Parser String)
unsignedDigits = define "unsignedDigits" $
  doc "Parse one or more decimal digits" $
  P.map @@ ("cs" ~> Strings.fromList (var "cs")) @@
    (P.some @@ (P.satisfy @@ ("c" ~> Logic.and (Ordering.gte (var "c") (int32 48)) (Ordering.lte (var "c") (int32 57)))))

-- | Parse a signed-or-unsigned run of decimal digits (an optional leading '-' per syntax.md #2.6:
-- "the character '-' occurs only inside numeric literals").
signedDigits :: TypedTermDefinition (Parser String)
signedDigits = define "signedDigits" $
  doc "Parse an optional leading '-' followed by one or more decimal digits" $
  P.bind @@ (P.optional @@ (P.char @@ int32 45)) @@ ("msign" ~>    -- '-'
    P.map @@ ("ds" ~> Optionals.match (var "msign")
        (var "ds")
        (constant $ Strings.concat2 (string "-") (var "ds"))) @@
      unsignedDigits)

-- | Parse the digit string of a JSON-grammar number (syntax.md #2.6): optional sign, integer
-- digits, optional fraction part, optional exponent part. The exponent part binds by maximal
-- munch -- there is no whitespace between the mantissa and 'e'/'E', so a single 'some' of
-- digit/'.'/exponent characters, assembled left-to-right without any intervening 'lexeme', gives
-- exactly that: '1.2e9' is one token, '1.2 e9' is '1.2' followed (after whitespace) by a separate
-- name 'e9'.
decimalDigits :: TypedTermDefinition (Parser String)
decimalDigits = define "decimalDigits" $
  doc "Parse the digit string of a JSON-grammar number: sign, integer, optional fraction, optional exponent" $
  P.bind @@ signedDigits @@ ("intPart" ~>
    P.bind @@ (P.optional @@ fracPart) @@ ("mfrac" ~>
      P.bind @@ (P.optional @@ expPart) @@ ("mexp" ~>
        P.pure @@ (Strings.concat $ list [
          var "intPart",
          Optionals.withDefault (string "") (var "mfrac"),
          Optionals.withDefault (string "") (var "mexp")]))))
  where
    fracPart = P.bind @@ (P.char @@ int32 46) @@ (constant $    -- '.'
      P.map @@ ("ds" ~> Strings.concat2 (string ".") (var "ds")) @@ unsignedDigits)
    expPart = P.bind @@ (P.satisfy @@ ("c" ~> Logic.or (Equality.equal (var "c") (int32 101)) (Equality.equal (var "c") (int32 69)))) @@ (constant $    -- 'e'/'E'
      P.bind @@ (P.optional @@ (P.satisfy @@ ("c" ~> Logic.or (Equality.equal (var "c") (int32 43)) (Equality.equal (var "c") (int32 45))))) @@ ("msign" ~>    -- '+'/'-'
        "signStr" <~ Optionals.match (var "msign") (string "") ("c" ~> Strings.fromList (list [var "c"])) $
        P.map @@ ("ds" ~> Strings.concat $ list [
          string "e",
          var "signStr",
          var "ds"]) @@ unsignedDigits))

-- | Parse the ':' width-suffix of an integer literal and build the corresponding IntegerValue by
-- parsing the preceding digit string with the matching parseIntN/parseUintN/parseBigint
-- primitive (syntax.md #2.6: "an unsuffixed numeric literal is always a decimal; integer and
-- float literals always carry a :width suffix" -- so the suffix determines which parse*
-- primitive validates and interprets the digits, not a separate numeric reader per width).
integerSuffix :: TypedTermDefinition (String -> Parser IntegerValue)
integerSuffix = define "integerSuffix" $
  doc "Parse the ':width' suffix of an integer literal and build its IntegerValue from the preceding digits" $
  "digits" ~>
  P.bind @@ (P.char @@ int32 58) @@ (constant $    -- ':'
    P.choice @@ list [
      suffixCase "bigint"  (\s -> Literals.parseBigint s)  Core.integerValueBigint,
      suffixCase "int8"    (\s -> Literals.parseInt8 s)    Core.integerValueInt8,
      suffixCase "int16"   (\s -> Literals.parseInt16 s)   Core.integerValueInt16,
      suffixCase "int32"   (\s -> Literals.parseInt32 s)   Core.integerValueInt32,
      suffixCase "int64"   (\s -> Literals.parseInt64 s)   Core.integerValueInt64,
      suffixCase "uint8"   (\s -> Literals.parseUint8 s)   Core.integerValueUint8,
      suffixCase "uint16"  (\s -> Literals.parseUint16 s)  Core.integerValueUint16,
      suffixCase "uint32"  (\s -> Literals.parseUint32 s)  Core.integerValueUint32,
      suffixCase "uint64"  (\s -> Literals.parseUint64 s)  Core.integerValueUint64])
  where
    suffixCase kw parse_ wrapIv =
      P.bind @@ (lexeme @@ (P.string_ @@ string kw)) @@ (constant $
        Optionals.match (parse_ (var "digits"))
          (P.fail @@ (Strings.concat $ list [string "invalid ", string kw, string " literal: ", var "digits"]))
          ("v" ~> P.pure @@ wrapIv (var "v")))

-- | Parse the ':' width-suffix of a float literal and build its FloatValue. Mirrors
-- 'integerSuffix' but for the two float widths; the digit string here may also be the bare
-- sentinel spellings 'NaN'/'Infinity'/'-Infinity' (syntax.md #2.6), which the digit-reading call
-- site is responsible for recognizing before calling this.
floatSuffix :: TypedTermDefinition (String -> Parser FloatValue)
floatSuffix = define "floatSuffix" $
  doc "Parse the ':width' suffix of a float literal and build its FloatValue from the preceding digits/sentinel" $
  "digits" ~>
  P.bind @@ (P.char @@ int32 58) @@ (constant $    -- ':'
    P.choice @@ list [
      suffixCase "float32" (\s -> Literals.parseFloat32 s) Core.floatValueFloat32,
      suffixCase "float64" (\s -> Literals.parseFloat64 s) Core.floatValueFloat64])
  where
    suffixCase kw parse_ wrapFv =
      P.bind @@ (lexeme @@ (P.string_ @@ string kw)) @@ (constant $
        Optionals.match (parse_ (var "digits"))
          (P.fail @@ (Strings.concat $ list [string "invalid ", string kw, string " literal: ", var "digits"]))
          ("v" ~> P.pure @@ wrapFv (var "v")))

-- | Parse a numeric literal: an integer, a float, or (with no ':width' suffix) a decimal, per
-- syntax.md #2.6's unambiguous-by-suffix rule. The non-finite float sentinels ('NaN',
-- 'Infinity', '-Infinity') are read as bare tokens ahead of the general digit reader, since they
-- are not digit strings. Negative-zero ('-0.0') falls out of the ordinary digit/decimal path (no
-- special case needed): 'signedDigits' accepts a leading '-', and 'parseFloat64'/'parseDecimal'
-- preserve the sign exactly as JSON numeric parsing does.
numericLit :: TypedTermDefinition (Parser Literal)
numericLit = define "numericLit" $
  doc "Parse a numeric literal: integer, float, or (unsuffixed) decimal" $
  P.alt
    @@ (P.bind @@ sentinelOrDigits @@ ("digits" ~>
          P.alt
            @@ (P.map @@ ("iv" ~> Core.literalInteger (var "iv")) @@ (integerSuffix @@ var "digits"))
            @@ (P.map @@ ("fv" ~> Core.literalFloat (var "fv")) @@ (floatSuffix @@ var "digits"))))
    @@ decimalOnly
  where
    sentinelOrDigits = P.choice @@ list [
      lexeme @@ (P.string_ @@ string "-Infinity"),
      lexeme @@ (P.string_ @@ string "Infinity"),
      lexeme @@ (P.string_ @@ string "NaN"),
      asTerm decimalDigits]
    decimalOnly = lexeme @@ (
      P.bind @@ decimalDigits @@ ("ds" ~>
        Optionals.match (Literals.parseDecimal (var "ds"))
          (P.fail @@ (Strings.concat2 (string "invalid decimal literal: ") (var "ds")))
          ("v" ~> P.pure @@ Core.literalDecimal (var "v"))))

-- | Parse a string literal (JSON string syntax per syntax.md #2.5).
stringLit :: TypedTermDefinition (Parser String)
stringLit = define "stringLit" $
  doc "Parse a JSON-syntax string literal" $
  lexeme @@ (
    P.bind @@ (P.char @@ int32 34) @@ (constant $    -- '"'
      P.bind @@ (P.many @@ strChar) @@ ("cs" ~>
        P.bind @@ (P.char @@ int32 34) @@ (constant $
          P.pure @@ (Strings.fromList $ var "cs")))))
  where
    strChar = P.alt
      @@ asTerm escapedChar
      @@ (P.satisfy @@ ("c" ~> Logic.and
            (Logic.not (Equality.equal (var "c") (int32 34)))    -- not '"'
            (Logic.not (Equality.equal (var "c") (int32 92)))))  -- not '\\'

-- | Parse a literal (syntax.md #2.6): boolean, numeric (integer/float/decimal), string, or
-- binary. Binary is a string literal with the ':binary' suffix; it is tried only after the
-- plain-string case fails to find that suffix, so an ordinary string literal is not mistaken for
-- one.
literalLit :: TypedTermDefinition (Parser Literal)
literalLit = define "literalLit" $
  doc "Parse a literal: boolean, numeric, string, or binary" $
  P.choice @@ list [
    P.bind @@ (termKeyword @@ string "true")  @@ (constant $ P.pure @@ Core.literalBoolean true),
    P.bind @@ (termKeyword @@ string "false") @@ (constant $ P.pure @@ Core.literalBoolean false),
    asTerm numericLit,
    P.bind @@ stringLit @@ ("s" ~>
      P.alt
        @@ (P.bind @@ (P.char @@ int32 58) @@ (constant $    -- ':'
             P.bind @@ (lexeme @@ (P.string_ @@ string "binary")) @@ (constant $
               P.pure @@ Core.literalBinary (Literals.base64ToBinary $ var "s"))))
        @@ (P.pure @@ Core.literalString (var "s")))]

-- | Parse a literal type keyword (syntax.md #3's literalType production): a base literal-type
-- keyword, or one of the integer/float width keywords.
literalTypeLit :: TypedTermDefinition (Parser LiteralType)
literalTypeLit = define "literalTypeLit" $
  doc "Parse a literal-type keyword: binary, boolean, decimal, string, an integer width, or a float width" $
  P.choice @@ list [
    kw "binary"  Core.literalTypeBinary,
    kw "boolean" Core.literalTypeBoolean,
    kw "decimal" Core.literalTypeDecimal,
    kw "string"  Core.literalTypeString,
    kw "bigint"  (Core.literalTypeInteger Core.integerTypeBigint),
    kw "int8"    (Core.literalTypeInteger Core.integerTypeInt8),
    kw "int16"   (Core.literalTypeInteger Core.integerTypeInt16),
    kw "int32"   (Core.literalTypeInteger Core.integerTypeInt32),
    kw "int64"   (Core.literalTypeInteger Core.integerTypeInt64),
    kw "uint8"   (Core.literalTypeInteger Core.integerTypeUint8),
    kw "uint16"  (Core.literalTypeInteger Core.integerTypeUint16),
    kw "uint32"  (Core.literalTypeInteger Core.integerTypeUint32),
    kw "uint64"  (Core.literalTypeInteger Core.integerTypeUint64),
    kw "float32" (Core.literalTypeFloat Core.floatTypeFloat32),
    kw "float64" (Core.literalTypeFloat Core.floatTypeFloat64)]
  where
    kw s v = P.bind @@ (typeKeyword @@ string s) @@ (constant $ P.pure @@ v)

-- | Parse a '{' name '}' term-fields block of exactly one field, for 'inject'/'unwrap'/'wrap'.
oneFieldBlock :: TypedTerm (Parser Field)
oneFieldBlock =
  P.bind @@ (termKeyword @@ string "{") @@ (constant $
    P.bind @@ (asTerm name) @@ ("fname" ~>
      P.bind @@ (termKeyword @@ string "=") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t" ~>
          P.bind @@ (termKeyword @@ string "}") @@ (constant $
            P.pure @@ Core.field (var "fname") (var "t"))))))

-- | Parse a termField ('name' '=' term) inside a termFields block.
termField :: TypedTermDefinition (Parser Field)
termField = define "termField" $
  doc "Parse a single term field: name '=' term" $
  P.bind @@ (asTerm name) @@ ("fname" ~>
    P.bind @@ (termKeyword @@ string "=") @@ (constant $
      P.map @@ ("t" ~> Core.field (var "fname") (var "t")) @@ (P.lazy @@ (constant $ asTerm term))))

-- | Parse a termFields block: '{' [termField (',' termField)*] '}'.
termFields :: TypedTermDefinition (Parser [Field])
termFields = define "termFields" $
  doc "Parse a brace-delimited, comma-separated list of term fields (possibly empty)" $
  P.bind @@ (termKeyword @@ string "{") @@ (constant $
    P.bind @@ (P.sepBy @@ (asTerm termField) @@ (termKeyword @@ string ",")) @@ ("flds" ~>
      P.bind @@ (termKeyword @@ string "}") @@ (constant $
        P.pure @@ var "flds")))

-- | Parse a 'name '(' name ')'' construct-head shared by case/inject/project/record/unwrap/wrap:
-- a keyword, then a parenthesized name.
parenName :: TypedTerm String -> TypedTerm (Parser Name)
parenName kw =
  P.bind @@ (termKeyword @@ kw) @@ (constant $
    P.bind @@ (termKeyword @@ string "(") @@ (constant $
      P.bind @@ (asTerm name) @@ ("n" ~>
        P.bind @@ (termKeyword @@ string ")") @@ (constant $
          P.pure @@ var "n"))))

-- | Parse a caseTerm: 'case' '(' name ')' termFields, with a trailing '_' field (if present)
-- extracted as the default handler (syntax.md #3: "the default handler, if present, appears
-- LAST as the wildcard field '_'").
caseTerm :: TypedTermDefinition (Parser Term)
caseTerm = define "caseTerm" $
  doc "Parse a case term: 'case' '(' name ')' termFields, splitting off a trailing '_' default field" $
  P.bind @@ (parenName (string "case")) @@ ("tname" ~>
    P.map @@ ("flds" ~>
      "isDefault" <~ ("f" ~> Equality.equal (Core.unName (Core.fieldName (var "f"))) (string "_")) $
      "defFields" <~ Lists.filter (var "isDefault") (var "flds") $
      "caseFields" <~ Lists.filter ("f" ~> Logic.not (var "isDefault" @@ var "f")) (var "flds") $
      "mdef" <~ Optionals.match (Lists.head (var "defFields")) nothing ("f" ~> just (Core.fieldTerm (var "f"))) $
      "alts" <~ Lists.map ("f" ~> Core.caseAlternative (Core.fieldName (var "f")) (Core.fieldTerm (var "f"))) (var "caseFields") $
      Core.termCases (Core.caseStatement (var "tname") (var "mdef") (var "alts"))) @@
    (asTerm termFields))

-- | Parse an eitherTerm: 'left' '(' term ')' | 'right' '(' term ')'.
eitherTerm :: TypedTermDefinition (Parser Term)
eitherTerm = define "eitherTerm" $
  doc "Parse an either term: 'left' '(' term ')' or 'right' '(' term ')'" $
  P.choice @@ list [
    P.map @@ ("t" ~> Core.termEither (asTerm (left (var "t")))) @@ side (string "left"),
    P.map @@ ("t" ~> Core.termEither (asTerm (right (var "t")))) @@ side (string "right")]
  where
    side :: TypedTerm String -> TypedTerm (Parser Term)
    side kw = P.bind @@ (termKeyword @@ kw) @@ (constant $
      P.bind @@ (termKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t" ~>
          P.bind @@ (termKeyword @@ string ")") @@ (constant $
            P.pure @@ var "t"))))

-- | Parse an injectTerm: 'inject' '(' name ')' termFields with exactly one field.
injectTerm :: TypedTermDefinition (Parser Term)
injectTerm = define "injectTerm" $
  doc "Parse an inject term: 'inject' '(' name ')' '{' name '=' term '}'" $
  P.bind @@ (parenName (string "inject")) @@ ("tname" ~>
    P.map @@ ("f" ~> Core.termInject (Core.injection (var "tname") (var "f"))) @@ oneFieldBlock)

-- | Parse a lambdaTerm: 'λ' binderName [':' type] '.' term (domain type optional).
lambdaTerm :: TypedTermDefinition (Parser Term)
lambdaTerm = define "lambdaTerm" $
  doc "Parse a lambda term: 'λ' binderName [':' type] '.' term" $
  P.bind @@ (lexeme @@ (P.char @@ int32 955)) @@ (constant $    -- 'λ'
    P.bind @@ (asTerm binderName) @@ ("v" ~>
      P.bind @@ (P.optional @@ domainType) @@ ("mt" ~>
        P.bind @@ (lexeme @@ (P.char @@ int32 46)) @@ (constant $    -- '.'
          P.map @@ ("body" ~> Core.termLambda (Core.lambda (var "v") (var "mt") (var "body"))) @@
            (P.lazy @@ (constant $ asTerm term))))))
  where
    domainType = P.bind @@ (lexeme @@ (P.char @@ int32 58)) @@ (constant $ asTerm type_)    -- ':'

-- | Parse a binding: name [':' '(' typeScheme ')'] ':=' term.
binding :: TypedTermDefinition (Parser Binding)
binding = define "binding" $
  doc "Parse a binding: name [':' '(' typeScheme ')'] ':=' term" $
  P.bind @@ (asTerm name) @@ ("n" ~>
    P.bind @@ (P.optional @@ ascribedScheme) @@ ("mts" ~>
      P.bind @@ (termKeyword @@ string ":=") @@ (constant $
        P.map @@ ("t" ~> Core.binding (var "n") (asTerm (var "t")) (var "mts")) @@
          (P.lazy @@ (constant $ asTerm term)))))
  where
    ascribedScheme =
      P.bind @@ (lexeme @@ (P.char @@ int32 58)) @@ (constant $    -- ':'
        P.bind @@ (termKeyword @@ string "(") @@ (constant $
          P.bind @@ (asTerm typeScheme) @@ ("ts" ~>
            P.bind @@ (termKeyword @@ string ")") @@ (constant $
              P.pure @@ var "ts"))))

-- | Parse a letTerm: 'let' binding (',' binding)* 'in' term.
letTerm :: TypedTermDefinition (Parser Term)
letTerm = define "letTerm" $
  doc "Parse a let term: 'let' binding (',' binding)* 'in' term" $
  P.bind @@ (termKeyword @@ string "let") @@ (constant $
    P.bind @@ (P.sepBy1 @@ (asTerm binding) @@ (termKeyword @@ string ",")) @@ ("bs" ~>
      P.bind @@ (termKeyword @@ string "in") @@ (constant $
        P.map @@ ("body" ~> Core.termLet (Core.let_ (var "bs") (var "body"))) @@
          (P.lazy @@ (constant $ asTerm term)))))

-- | Parse a listTerm: '[' [term (',' term)*] ']'.
listTerm :: TypedTermDefinition (Parser Term)
listTerm = define "listTerm" $
  doc "Parse a list term: '[' [term (',' term)*] ']'" $
  P.bind @@ (termKeyword @@ string "[") @@ (constant $
    P.bind @@ (P.sepBy @@ (P.lazy @@ (constant $ asTerm term)) @@ (termKeyword @@ string ",")) @@ ("els" ~>
      P.bind @@ (termKeyword @@ string "]") @@ (constant $
        P.pure @@ Core.termList (var "els"))))

-- | Parse an optionalTerm: 'given' '(' term ')' | 'none'.
optionalTerm :: TypedTermDefinition (Parser Term)
optionalTerm = define "optionalTerm" $
  doc "Parse an optional term: 'given' '(' term ')' or 'none'" $
  P.choice @@ list [
    P.bind @@ (termKeyword @@ string "none") @@ (constant $ P.pure @@ Core.termOptional nothing),
    P.bind @@ (termKeyword @@ string "given") @@ (constant $
      P.bind @@ (termKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t" ~>
          P.bind @@ (termKeyword @@ string ")") @@ (constant $
            P.pure @@ Core.termOptional (just (var "t"))))))]

-- | Parse a projectTerm: 'project' '(' name ')' '{' name '}'.
projectTerm :: TypedTermDefinition (Parser Term)
projectTerm = define "projectTerm" $
  doc "Parse a project term: 'project' '(' name ')' '{' name '}'" $
  P.bind @@ (parenName (string "project")) @@ ("tname" ~>
    P.bind @@ (termKeyword @@ string "{") @@ (constant $
      P.bind @@ (asTerm name) @@ ("fname" ~>
        P.bind @@ (termKeyword @@ string "}") @@ (constant $
          P.pure @@ Core.termProject (Core.projection (var "tname") (var "fname"))))))

-- | Parse a recordTerm: 'record' '(' name ')' termFields.
recordTerm :: TypedTermDefinition (Parser Term)
recordTerm = define "recordTerm" $
  doc "Parse a record term: 'record' '(' name ')' '{' [termField (',' termField)*] '}'" $
  P.bind @@ (parenName (string "record")) @@ ("tname" ~>
    P.map @@ ("flds" ~> Core.termRecord (Core.record (var "tname") (var "flds"))) @@ (asTerm termFields))

-- | Parse an unwrapTerm: 'unwrap' '(' name ')'.
unwrapTerm :: TypedTermDefinition (Parser Term)
unwrapTerm = define "unwrapTerm" $
  doc "Parse an unwrap term: 'unwrap' '(' name ')'" $
  P.map @@ ("n" ~> Core.termUnwrap (var "n")) @@ (parenName (string "unwrap"))

-- | Parse a wrapTerm: 'wrap' '(' name ')' '{' term '}'.
wrapTerm :: TypedTermDefinition (Parser Term)
wrapTerm = define "wrapTerm" $
  doc "Parse a wrap term: 'wrap' '(' name ')' '{' term '}'" $
  P.bind @@ (parenName (string "wrap")) @@ ("tname" ~>
    P.bind @@ (termKeyword @@ string "{") @@ (constant $
      P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t" ~>
        P.bind @@ (termKeyword @@ string "}") @@ (constant $
          P.pure @@ Core.termWrap (Core.wrappedTerm (var "tname") (var "t"))))))

-- | Parse a braced '{' ... '}' construct that is either a set or a map (syntax.md #3: "nonempty
-- braces are a map exactly when entries contain a top-level '='"), with one token of lookahead
-- inside the braces to decide which. '{}' is the empty set; '{=}' is the empty map.
braceContent :: TypedTermDefinition (Parser Term)
braceContent = define "braceContent" $
  doc "Parse '{' ... '}' as a set or map term, disambiguating by a top-level '=' after the first element" $
  P.bind @@ (termKeyword @@ string "{") @@ (constant $
    P.choice @@ list [
      -- '{=}': the empty map
      P.bind @@ (termKeyword @@ string "=") @@ (constant $
        P.bind @@ (termKeyword @@ string "}") @@ (constant $
          P.pure @@ Core.termMap (Maps.fromList (list ([] :: [TypedTerm (Term, Term)]))))),
      -- '{}': the empty set
      P.bind @@ (termKeyword @@ string "}") @@ (constant $
        P.pure @@ Core.termSet (Sets.fromList (list ([] :: [TypedTerm Term])))),
      -- nonempty: parse one term, then look for '=' (map) vs ',' or '}' (set)
      P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t1" ~>
        P.alt
          @@ (P.bind @@ (termKeyword @@ string "=") @@ (constant $
                P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("v1" ~>
                  P.bind @@ (P.many @@ mapEntryTail) @@ ("rest" ~>
                    P.bind @@ (termKeyword @@ string "}") @@ (constant $
                      P.pure @@ Core.termMap (Maps.fromList (Lists.cons (pair (var "t1") (var "v1")) (var "rest"))))))))
          @@ (P.bind @@ (P.many @@ setElementTail) @@ ("rest" ~>
                P.bind @@ (termKeyword @@ string "}") @@ (constant $
                  P.pure @@ Core.termSet (Sets.fromList (Lists.cons (var "t1") (var "rest"))))))) ])
  where
    mapEntryTail = P.bind @@ (termKeyword @@ string ",") @@ (constant $
      P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("k" ~>
        P.bind @@ (termKeyword @@ string "=") @@ (constant $
          P.map @@ ("v" ~> pair (var "k") (var "v")) @@ (P.lazy @@ (constant $ asTerm term)))))
    setElementTail = P.bind @@ (termKeyword @@ string ",") @@ (constant $
      P.lazy @@ (constant $ asTerm term))

-- | Parse an atomicTerm: any term construct other than application/annotation/type-application
-- (those are built on top of this by 'postfixTerm'/the application loop), plus parenthesized
-- grouping and pairing.
atomicTerm :: TypedTermDefinition (Parser Term)
atomicTerm = define "atomicTerm" $
  doc "Parse a tightest-level term: any construct other than application, annotation, or type application" $
  P.choice @@ list [
    asTerm caseTerm,
    asTerm eitherTerm,
    asTerm injectTerm,
    asTerm lambdaTerm,
    asTerm letTerm,
    asTerm listTerm,
    P.map @@ ("l" ~> Core.termLiteral (var "l")) @@ (asTerm literalLit),
    asTerm braceContent,
    asTerm optionalTerm,
    asTerm projectTerm,
    asTerm recordTerm,
    typeLambdaTermParser,
    P.bind @@ (termKeyword @@ string "unit") @@ (constant $ P.pure @@ Core.termUnit),
    asTerm unwrapTerm,
    asTerm wrapTerm,
    parenOrPair,
    P.map @@ ("n" ~> Core.termVariable (var "n")) @@ (asTerm name)]
  where
    typeLambdaTermParser =
      P.bind @@ (lexeme @@ (P.char @@ int32 923)) @@ (constant $    -- 'Λ'
        P.bind @@ (asTerm binderName) @@ ("v" ~>
          P.bind @@ (lexeme @@ (P.char @@ int32 46)) @@ (constant $    -- '.'
            P.map @@ ("body" ~> Core.termTypeLambda (Core.typeLambda (var "v") (var "body"))) @@
              (P.lazy @@ (constant $ asTerm term)))))
    parenOrPair =
      P.bind @@ (termKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t1" ~>
          P.alt
            @@ (P.bind @@ (termKeyword @@ string ",") @@ (constant $
                  P.bind @@ (P.lazy @@ (constant $ asTerm term)) @@ ("t2" ~>
                    P.bind @@ (termKeyword @@ string ")") @@ (constant $
                      P.pure @@ Core.termPair (pair (var "t1") (var "t2"))))))
            @@ (P.bind @@ (termKeyword @@ string ")") @@ (constant $
                  P.pure @@ var "t1"))))

-- | Parse the postfix level (syntax.md #3 precedence 1): an atomicTerm, then zero or more
-- trailing '@' annotation / '⟨type⟩' type-application suffixes, left-chaining (an annotated or
-- type-applied term is itself atomic-at-this-level for the next suffix, so 'x @m1 @m2' chains
-- left as annotated(annotated(x, m1), m2), matching PrintCore's own chains-left behavior).
-- Each suffix is parsed as 'Either Term Type' (an annotation payload is a Term; a type-
-- application payload is a Type) so both suffix kinds can be collected by one 'P.many' call and
-- then left-folded in parse order.
postfixTerm :: TypedTermDefinition (Parser Term)
postfixTerm = define "postfixTerm" $
  doc "Parse an atomic term followed by zero or more postfix '@annotation'/'⟨type⟩' suffixes, left-chaining" $
  P.bind @@ (asTerm atomicTerm) @@ ("t0" ~>
    P.map @@ ("sufs" ~> Lists.foldl (asTerm applySuffix) (var "t0") (var "sufs")) @@
      (P.many @@ (P.alt @@ (P.map @@ (asTerm left_) @@ (asTerm annotSuffix)) @@ (P.map @@ (asTerm right_) @@ (asTerm typeApplicationSuffix)))))
  where
    applySuffix = "t" ~> "suf" ~> Eithers.either
      ("ann" ~> Core.termAnnotated (Core.annotatedTerm (var "t") (var "ann")))
      ("typ" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "t") (var "typ")))
      (var "suf")

-- | Parse a top-level term: the full postfix term, with no further annotation/application
-- wrapping expected at this exact call site (used as the entry point for nested term positions
-- that are themselves already at the tightest precedence, e.g. inside '(' ... ')').
topTerm :: TypedTermDefinition (Parser Term)
topTerm = define "topTerm" $
  doc "Parse a term at postfix precedence (an atomic term plus its postfix suffixes)" $
  asTerm postfixTerm

-- | Parse the application level (syntax.md #3 precedence 2): one postfix-level term, then
-- greedily more, separated only by whitespace, left-folding into nested Application terms.
-- Termination is purely "the next postfixTerm parse failed" (standard PEG-style adjacency
-- parsing) -- every term-starting keyword is itself a valid atom start, so no special lookahead
-- table is needed (per the design doc's combinator-fit analysis).
applicationLevel :: TypedTerm (Parser Term)
applicationLevel =
  P.bind @@ (asTerm postfixTerm) @@ ("t0" ~>
    P.map @@ ("rest" ~> Lists.foldl (asTerm applyLeft) (var "t0") (var "rest")) @@
      (P.many @@ (asTerm postfixTerm)))
  where
    applyLeft = "f" ~> "a" ~> Core.termApplication (Core.application (var "f") (var "a"))

-- | Parse a term (the full grammar, top-level entry point): the application level.
-- NOTE: this entry point does not itself implement the '@'/'⟨⟩' postfix loop or the right-hand
-- side of '->' (types only) -- those are folded into 'postfixTerm' (for '@'/'⟨⟩') and into
-- typeFunctionLevel (for types). Term grammar has no infix operators above application per the
-- precedence table, so 'term' is exactly 'applicationLevel'.
term :: TypedTermDefinition (Parser Term)
term = define "term" $
  doc "Parse a full term: application of postfix-level terms" $
  applicationLevel

-- | Run the term parser on a full input string, requiring all input to be consumed (after
-- trailing whitespace/comments). Returns the parse error message on failure or trailing input.
parseTerm :: TypedTermDefinition (String -> Either String Term)
parseTerm = define "parseTerm" $
  doc "Parse a complete term from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm term)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after term")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]

----------------------------------------------------------------------------------------------
-- Types

-- | Parse a typeFields block: '{' [typeField (',' typeField)*] '}', typeField ::= name ':' type.
typeFieldsBlock :: TypedTerm (Parser [FieldType])
typeFieldsBlock =
  P.bind @@ (typeKeyword @@ string "{") @@ (constant $
    P.bind @@ (P.sepBy @@ typeFieldParser @@ (typeKeyword @@ string ",")) @@ ("flds" ~>
      P.bind @@ (typeKeyword @@ string "}") @@ (constant $
        P.pure @@ var "flds")))
  where
    typeFieldParser =
      P.bind @@ (asTerm name) @@ ("fname" ~>
        P.bind @@ (typeKeyword @@ string ":") @@ (constant $
          P.map @@ ("t" ~> Core.fieldType (var "fname") (var "t")) @@ (P.lazy @@ (constant $ asTerm type_))))

-- | Parse an 'angleType' suffix: '<' type '>', used by effect/list/optional/set type constructs.
angleType :: TypedTerm (Parser Type)
angleType =
  P.bind @@ (typeKeyword @@ string "<") @@ (constant $
    P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("t" ~>
      P.bind @@ (typeKeyword @@ string ">") @@ (constant $
        P.pure @@ var "t")))

-- | Parse an atomicType: any type construct other than application/annotation/function (those
-- are built on top of this), plus parenthesized grouping and pairing.
typeAtom :: TypedTermDefinition (Parser Type)
typeAtom = define "typeAtom" $
  doc "Parse a tightest-level type: any construct other than application, annotation, or function" $
  P.choice @@ list [
    P.map @@ ("lt" ~> Core.typeLiteral (var "lt")) @@ (asTerm literalTypeLit),
    effectT, eitherT, forallT, listT, mapT, optionalT, recordT, setT, unionT,
    P.bind @@ (typeKeyword @@ string "unit") @@ (constant $ P.pure @@ Core.typeUnit),
    P.bind @@ (typeKeyword @@ string "void") @@ (constant $ P.pure @@ Core.typeVoid),
    wrapT,
    parenOrPairType,
    P.map @@ ("n" ~> Core.typeVariable (var "n")) @@ (asTerm name)]
  where
    effectT = P.bind @@ (typeKeyword @@ string "effect") @@ (constant $ P.map @@ ("t" ~> Core.typeEffect (var "t")) @@ angleType)
    listT = P.bind @@ (typeKeyword @@ string "list") @@ (constant $ P.map @@ ("t" ~> Core.typeList (var "t")) @@ angleType)
    optionalT = P.bind @@ (typeKeyword @@ string "optional") @@ (constant $ P.map @@ ("t" ~> Core.typeOptional (var "t")) @@ angleType)
    setT = P.bind @@ (typeKeyword @@ string "set") @@ (constant $ P.map @@ ("t" ~> Core.typeSet (var "t")) @@ angleType)
    eitherT = P.bind @@ (typeKeyword @@ string "either") @@ (constant $
      P.bind @@ (typeKeyword @@ string "<") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("l" ~>
          P.bind @@ (typeKeyword @@ string ",") @@ (constant $
            P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("r" ~>
              P.bind @@ (typeKeyword @@ string ">") @@ (constant $
                P.pure @@ Core.typeEither (Core.eitherType (var "l") (var "r"))))))))
    mapT = P.bind @@ (typeKeyword @@ string "map") @@ (constant $
      P.bind @@ (typeKeyword @@ string "<") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("k" ~>
          P.bind @@ (typeKeyword @@ string ",") @@ (constant $
            P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("v" ~>
              P.bind @@ (typeKeyword @@ string ">") @@ (constant $
                P.pure @@ Core.typeMap (Core.mapType (var "k") (var "v"))))))))
    forallT = P.bind @@ (typeKeyword @@ string "forall") @@ (constant $
      P.bind @@ (asTerm binderName) @@ ("v" ~>
        P.bind @@ (typeKeyword @@ string ".") @@ (constant $
          P.map @@ ("body" ~> Core.typeForall (Core.forallType (var "v") (var "body"))) @@
            (P.lazy @@ (constant $ asTerm type_)))))
    recordT = P.bind @@ (typeKeyword @@ string "record") @@ (constant $ P.map @@ ("flds" ~> Core.typeRecord (var "flds")) @@ typeFieldsBlock)
    unionT = P.bind @@ (typeKeyword @@ string "union") @@ (constant $ P.map @@ ("flds" ~> Core.typeUnion (var "flds")) @@ typeFieldsBlock)
    wrapT = P.bind @@ (typeKeyword @@ string "wrap") @@ (constant $
      P.bind @@ (typeKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("t" ~>
          P.bind @@ (typeKeyword @@ string ")") @@ (constant $
            P.pure @@ Core.typeWrap (var "t")))))
    parenOrPairType = P.bind @@ (typeKeyword @@ string "(") @@ (constant $
      P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("t1" ~>
        P.alt
          @@ (P.bind @@ (typeKeyword @@ string ",") @@ (constant $
                P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("t2" ~>
                  P.bind @@ (typeKeyword @@ string ")") @@ (constant $
                    P.pure @@ Core.typePair (Core.pairType (var "t1") (var "t2"))))))
          @@ (P.bind @@ (typeKeyword @@ string ")") @@ (constant $
                P.pure @@ var "t1"))))

-- | Parse the postfix level for types: a typeAtom, then zero or more trailing '@' annotation
-- suffixes (types have no '⟨⟩' type-application postfix of their own -- that notation belongs
-- to terms).
postfixType :: TypedTermDefinition (Parser Type)
postfixType = define "postfixType" $
  doc "Parse an atomic type followed by zero or more trailing '@annotation' suffixes" $
  asTerm typeAtom

-- | Parse the application level for types (syntax.md #3 precedence 2): one postfix-level type,
-- then greedily more, left-folding into nested ApplicationType.
typeApplicationLevel :: TypedTerm (Parser Type)
typeApplicationLevel =
  P.bind @@ (asTerm postfixType) @@ ("t0" ~>
    P.map @@ ("rest" ~> Lists.foldl (asTerm applyLeft) (var "t0") (var "rest")) @@
      (P.many @@ (asTerm postfixType)))
  where
    applyLeft = "f" ~> "a" ~> Core.typeApplication (Core.applicationType (var "f") (var "a"))

-- | Parse the function-arrow level for types (syntax.md #3 precedence 3): one application-level
-- type, then if '→' follows, recurse for the right side and build FunctionType -- right-
-- associative via ordinary recursive descent (no left-recursion risk on the right side). Named
-- 'typeFunctionLevel', not 'typeFunction', to avoid colliding with the Model constructor
-- typeFunction (FunctionType -> Type).
typeFunctionLevel :: TypedTermDefinition (Parser Type)
typeFunctionLevel = define "typeFunction" $
  doc "Parse a function type: application-level type, optionally followed by '→' and a recursive right side" $
  P.bind @@ typeApplicationLevel @@ ("dom" ~>
    P.alt
      @@ (P.bind @@ (lexeme @@ (P.char @@ int32 8594)) @@ (constant $    -- '→'
            P.map @@ ("cod" ~> mkFunctionType @@ var "dom" @@ var "cod") @@
              (P.lazy @@ (constant $ asTerm typeFunctionLevel))))
      @@ (P.pure @@ var "dom"))
  where
    mkFunctionType = "d" ~> "c" ~> Core.typeFunction (Core.functionType (var "d") (var "c"))

-- | Parse a type application suffix on a term: '⟨' type '⟩'. Exposed separately from
-- 'postfixTerm' since the application-term postfix loop needs to try this alongside '@'
-- annotation at the same precedence level.
typeApplicationSuffix :: TypedTermDefinition (Parser Type)
typeApplicationSuffix = define "typeApplicationSuffix" $
  doc "Parse a term-level type-application suffix: '⟨' type '⟩'" $
  P.bind @@ (lexeme @@ (P.char @@ int32 10216)) @@ (constant $    -- '⟨'
    P.bind @@ (P.lazy @@ (constant $ asTerm type_)) @@ ("t" ~>
      P.bind @@ (lexeme @@ (P.char @@ int32 10217)) @@ (constant $    -- '⟩'
        P.pure @@ var "t")))

-- | Parse an '@' annotation suffix (shared by terms and types): '@' term. The payload is parsed
-- at the tightest (postfix) level, per syntax.md #3's note that larger payloads must be
-- parenthesized by the writer.
annotSuffix :: TypedTermDefinition (Parser Term)
annotSuffix = define "annot" $
  doc "Parse a term-level or type-level '@' annotation suffix" $
  P.bind @@ (lexeme @@ (P.char @@ int32 64)) @@ (constant $    -- '@'
    asTerm postfixTerm)

-- | Parse a top-level type: the full function-arrow level.
topType :: TypedTermDefinition (Parser Type)
topType = define "topType" $
  doc "Parse a type at function-arrow precedence (the full type grammar)" $
  asTerm typeFunctionLevel

-- | Parse a type (the full grammar, top-level entry point).
type_ :: TypedTermDefinition (Parser Type)
type_ = define "type" $
  doc "Parse a full type" $
  asTerm typeFunctionLevel

-- | Run the type parser on a full input string, requiring all input to be consumed.
parseType :: TypedTermDefinition (String -> Either String Type)
parseType = define "parseType" $
  doc "Parse a complete type from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm type_)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after type")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]

----------------------------------------------------------------------------------------------
-- Type schemes

-- | Parse a constraint: className typeVariable (e.g. 'ordering a'). The class name is read as a
-- bare name (a single name token, not the general dotted-greedy 'name' parser, since a
-- constraint's class name is immediately followed by its type-variable argument with no
-- separator other than whitespace).
constraint :: TypedTermDefinition (Parser (Name, Name))
constraint = define "constraint" $
  doc "Parse a type-class constraint: className typeVariable" $
  P.bind @@ (asTerm name) @@ ("cls" ~>
    P.map @@ ("v" ~> pair (var "cls") (var "v")) @@ (asTerm name))

-- | Parse a typeScheme: '∀' [binderName (',' binderName)*] '.' [schemeConstraints '⇒'] type.
-- '∀.t' is a monomorphic scheme (empty binder list).
typeScheme :: TypedTermDefinition (Parser TypeScheme)
typeScheme = define "typeScheme" $
  doc "Parse a type scheme: '∀' [binderName (',' binderName)*] '.' [constraints '⇒'] type" $
  P.bind @@ (lexeme @@ (P.char @@ int32 8704)) @@ (constant $    -- '∀'
    P.bind @@ (P.sepBy @@ (asTerm binderName) @@ (lexeme @@ (P.char @@ int32 44))) @@ ("vars" ~>    -- ','
      P.bind @@ (lexeme @@ (P.char @@ int32 46)) @@ (constant $    -- '.'
        P.bind @@ (P.optional @@ constraintsBlock) @@ ("mcs" ~>
          P.map @@ ("body" ~> Core.typeScheme (var "vars") (var "body")
              (Maps.fromList (Lists.map (asTerm toConstraintsEntry) (Optionals.withDefault (list ([] :: [TypedTerm (Name, Name)])) (var "mcs"))))) @@
            (P.lazy @@ (constant $ asTerm type_))))))
  where
    constraintsBlock =
      P.bind @@ (termKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.sepBy1 @@ (asTerm constraint) @@ (termKeyword @@ string ",")) @@ ("cs" ~>
          P.bind @@ (termKeyword @@ string ")") @@ (constant $
            P.bind @@ (lexeme @@ (P.char @@ int32 8658)) @@ (constant $    -- '⇒'
              P.pure @@ var "cs"))))
    -- Group constraints by type variable: multiple (class, var) pairs sharing a var collapse
    -- into one TypeVariableConstraints with a multi-element classes set.
    toConstraintsEntry = "p" ~> pair (Pairs.second (var "p"))
      (Core.typeVariableConstraints (Sets.fromList (list [Core.typeClassConstraintSimple (Pairs.first (var "p"))])))

-- | Parse one element of a term-signature arrow chain: an optional 'lazy' keyword (#800) followed
-- by an application-level type, yielding (isLazy, type).
lazyTypeApplicationLevel :: TypedTerm (Parser (Bool, Type))
lazyTypeApplicationLevel =
  P.bind @@ (P.optional @@ (typeKeyword @@ string "lazy")) @@ ("mlazy" ~>
    P.map @@ ("t" ~> pair (Optionals.isGiven $ var "mlazy") (var "t")) @@ (asTerm typeApplicationLevel))

-- | Parse the arrow chain of a term signature (syntax.md #3 precedence 3, signature variant):
-- one '[lazy] application-level type', then if '→' follows, recurse for the right side --
-- right-associative, mirroring typeFunctionLevel, but gathering (isLazy, type) parameter pairs
-- in REVERSE order alongside the final result type, instead of building a Type, since only a
-- term signature's Parameters (not Type.function) can carry a laziness flag (#800). Mirrors the
-- "peel" accumulator in Scoping.typeSchemeToTermSignature, but built directly during parsing
-- (right-associative recursion naturally yields the result type first, then each domain
-- prepended as recursion unwinds) rather than by peeling a pre-built Type afterward.
-- NOTE: must be a registered 'define'd TypedTermDefinition, NOT a bare TypedTerm, because it
-- self-references recursively below. AsTerm's instance for TypedTermDefinition converts the
-- self-reference to a lightweight TermVariable; the instance for a bare TypedTerm is 'id', which
-- would embed this term's entire (self-referencing) body inline at the recursive call site --
-- an infinite Haskell-level term graph that overflows the stack the instant anything traverses it
-- (discovered via #800's own stack-space-overflow failure in checkUndeclaredDependencies).
termSignatureArrowChain :: TypedTermDefinition (Parser ([(Bool, Type)], Type))
termSignatureArrowChain = define "termSignatureArrowChain" $
  doc "Parse the arrow chain of a term signature, gathering (isLazy, type) parameter pairs alongside the final result type" $
  P.bind @@ (asTerm lazyTypeApplicationLevel) @@ ("dom" ~>
    P.alt
      @@ (P.bind @@ (lexeme @@ (P.char @@ int32 8594)) @@ (constant $    -- '→'
            P.map @@ ("rest" ~> pair
                (Lists.cons (var "dom") (Pairs.first (var "rest" :: TypedTerm ([(Bool, Type)], Type))))
                (Pairs.second (var "rest"))) @@
              (P.lazy @@ (constant $ asTerm termSignatureArrowChain))))
      @@ (P.pure @@ pair (list ([] :: [TypedTerm (Bool, Type)])) (Pairs.second (var "dom"))))

-- | Parse a term signature: '∀' [binderName (',' binderName)*] '.' [schemeConstraints '⇒']
-- arrow chain, where each parameter position (all but the last) may carry a 'lazy' keyword
-- (#800), setting that Parameter's isLazy. Value-parameter names are synthesized as
-- arg0, arg1, ..., mirroring Scoping.typeSchemeToTermSignature's convention -- this is the
-- parser counterpart used at term/primitive-definition sites (Parse/Packaging.hs), deliberately
-- bypassing the lossy TypeScheme->TermSignature bridge (Scoping.hs), which always defaults
-- isLazy to false. The header (vars/constraints) parsing is identical to typeScheme's.
termSignature :: TypedTermDefinition (Parser TermSignature)
termSignature = define "termSignature" $
  doc "Parse a term signature: '∀' [binderName (',' binderName)*] '.' [constraints '⇒'] arrow chain, with optional 'lazy' markers on parameter positions" $
  P.bind @@ (lexeme @@ (P.char @@ int32 8704)) @@ (constant $    -- '∀'
    P.bind @@ (P.sepBy @@ (asTerm binderName) @@ (lexeme @@ (P.char @@ int32 44))) @@ ("vars" ~>    -- ','
      P.bind @@ (lexeme @@ (P.char @@ int32 46)) @@ (constant $    -- '.'
        P.bind @@ (P.optional @@ constraintsBlock) @@ ("mcs" ~>
          "typeParams" <~ Lists.map
            ("v" ~> Typing.typeParameter (var "v")
              (Optionals.withDefault (list ([] :: [TypedTerm TypeClassConstraint]))
                (Maps.lookup (var "v" :: TypedTerm Name)
                  (Maps.fromList (Lists.map (asTerm toTypeParamConstraintsEntry) (Optionals.withDefault (list ([] :: [TypedTerm (Name, Name)])) (var "mcs")))))))
            (var "vars") $
          P.map @@ ("chain" ~>
              -- The chain accumulator is already in forward (left-to-right, domain-first) order:
              -- each level of the right-associative recursion prepends its OWN (outer, earlier)
              -- domain onto the inner recursive result as it returns, so the leftmost-parsed
              -- domain ends up at the head of the list.
              "fwdParams" <~ Pairs.first (var "chain" :: TypedTerm ([(Bool, Type)], Type)) $
              "resultType" <~ Pairs.second (var "chain") $
              -- Build Parameters with synthetic names arg0, arg1, ... via a (length-1)-indexed
              -- fold, since there is no DSL `range` helper -- same idiom as
              -- Scoping.typeSchemeToTermSignature.
              "params" <~ Lists.reverse (Pairs.first $ Lists.foldl
                ("acc" ~> "item" ~>
                  "pairAcc" <~ Pairs.first (var "acc") $
                  "i" <~ Pairs.second (var "acc") $
                  pair
                    (Lists.cons
                      (Typing.parameter
                        (Core.name $ Strings.concat (list [string "arg", Literals.printInt32 (var "i")]))
                        Phantoms.nothing
                        (Pairs.second (var "item" :: TypedTerm (Bool, Type)))
                        (Pairs.first (var "item")))
                      (var "pairAcc"))
                    (Math.add (var "i") (int32 1)))
                (pair (list ([] :: [TypedTerm Parameter])) (int32 0))
                (var "fwdParams")) $
              "result" <~ Typing.result Phantoms.nothing (var "resultType") $
              Typing.termSignature (var "typeParams") (var "params") (var "result")) @@
            (asTerm termSignatureArrowChain)))))
  where
    constraintsBlock =
      P.bind @@ (termKeyword @@ string "(") @@ (constant $
        P.bind @@ (P.sepBy1 @@ (asTerm constraint) @@ (termKeyword @@ string ",")) @@ ("cs" ~>
          P.bind @@ (termKeyword @@ string ")") @@ (constant $
            P.bind @@ (lexeme @@ (P.char @@ int32 8658)) @@ (constant $    -- '⇒'
              P.pure @@ var "cs"))))
    -- Map each type variable to its constraints, keyed by variable name. Mirrors typeScheme's own
    -- toConstraintsEntry/Maps.fromList pattern exactly, including its pre-existing limitation:
    -- multiple (class, var) pairs sharing a variable are not merged, since Maps.fromList keeps
    -- only the last entry per key -- out of scope for #800, which concerns 'lazy' only.
    toTypeParamConstraintsEntry = "p" ~> pair (Pairs.second (var "p"))
      (list [Core.typeClassConstraintSimple (Pairs.first (var "p"))])

-- | Run the term-signature parser on a full input string, requiring all input to be consumed.
parseTermSignature :: TypedTermDefinition (String -> Either String TermSignature)
parseTermSignature = define "parseTermSignature" $
  doc "Parse a complete term signature from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm termSignature)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after term signature")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]

-- | Run the type-scheme parser on a full input string, requiring all input to be consumed.
parseTypeScheme :: TypedTermDefinition (String -> Either String TypeScheme)
parseTypeScheme = define "parseTypeScheme" $
  doc "Parse a complete type scheme from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm typeScheme)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after type scheme")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]
