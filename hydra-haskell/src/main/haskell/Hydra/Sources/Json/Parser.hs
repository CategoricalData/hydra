
module Hydra.Sources.Json.Parser where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import qualified Hydra.Dsl.Meta.Parsing       as Parsing
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Parsers         as Parsers
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

import qualified Hydra.Json.Model as J


jsonParserDefinition :: String -> TTerm a -> TBinding a
jsonParserDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [Parsers.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON parser using Hydra parser combinators"
  where
    ns = Namespace "hydra.json.parser"
    elements = [
      toBinding whitespace,
      toBinding token,
      toBinding jsonNull,
      toBinding jsonBool,
      toBinding digit,
      toBinding digits,
      toBinding jsonIntegerPart,
      toBinding jsonFractionPart,
      toBinding jsonExponentPart,
      toBinding jsonNumber,
      toBinding jsonEscapeChar,
      toBinding jsonStringChar,
      toBinding jsonString,
      toBinding jsonArray,
      toBinding jsonKeyValue,
      toBinding jsonObject,
      toBinding jsonValue,
      toBinding parseJson]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | ASCII code points for common characters
spaceCode, tabCode, newlineCode, returnCode :: TTerm Int
spaceCode = int32 32      -- ' '
tabCode = int32 9         -- '\t'
newlineCode = int32 10    -- '\n'
returnCode = int32 13     -- '\r'

quoteCode, backslashCode, colonCode, commaCode :: TTerm Int
quoteCode = int32 34      -- '"'
backslashCode = int32 92  -- '\\'
colonCode = int32 58      -- ':'
commaCode = int32 44      -- ','

bracketOpenCode, bracketCloseCode, braceOpenCode, braceCloseCode :: TTerm Int
bracketOpenCode = int32 91   -- '['
bracketCloseCode = int32 93  -- ']'
braceOpenCode = int32 123    -- '{'
braceCloseCode = int32 125   -- '}'

zeroCode, nineCode, minusCode, plusCode, dotCode :: TTerm Int
zeroCode = int32 48       -- '0'
nineCode = int32 57       -- '9'
minusCode = int32 45      -- '-'
plusCode = int32 43       -- '+'
dotCode = int32 46        -- '.'

letterELower, letterEUpper :: TTerm Int
letterELower = int32 101  -- 'e'
letterEUpper = int32 69   -- 'E'

letterNCode, letterUCode, letterLCode :: TTerm Int
letterNCode = int32 110   -- 'n'
letterUCode = int32 117   -- 'u'
letterLCode = int32 108   -- 'l'

letterTCode, letterRCode, letterFCode, letterACode, letterSCode :: TTerm Int
letterTCode = int32 116   -- 't'
letterRCode = int32 114   -- 'r'
letterFCode = int32 102   -- 'f'
letterACode = int32 97    -- 'a'
letterSCode = int32 115   -- 's'

letterBCode :: TTerm Int
letterBCode = int32 98    -- 'b'

-- | Parse zero or more whitespace characters
whitespace :: TBinding (Parser ())
whitespace = define "whitespace" $
  doc "Parse zero or more JSON whitespace characters (space, tab, newline, carriage return)" $
  Parsers.map @@ (constant unit) @@
    (Parsers.many @@
      (Parsers.satisfy @@ ("c" ~>
        Logic.ors (list [
          Equality.equal (var "c") spaceCode,
          Equality.equal (var "c") tabCode,
          Equality.equal (var "c") newlineCode,
          Equality.equal (var "c") returnCode]))))

-- | Parse a token followed by optional whitespace
token :: TBinding (Parser a -> Parser a)
token = define "token" $
  doc "Parse a token followed by optional whitespace" $
  "p" ~>
    Parsers.bind @@ var "p" @@ ("x" ~>
      Parsers.bind @@ whitespace @@ (constant $
        Parsers.pure @@ var "x"))

-- | Parse JSON null
jsonNull :: TBinding (Parser J.Value)
jsonNull = define "jsonNull" $
  doc "Parse JSON null value" $
  Parsers.map @@ (constant Json.valueNull) @@
    (token @@ (Parsers.string_ @@ string "null"))

-- | Parse JSON boolean
jsonBool :: TBinding (Parser J.Value)
jsonBool = define "jsonBool" $
  doc "Parse JSON boolean (true or false)" $
  Parsers.alt
    @@ (Parsers.map @@ (constant $ Json.valueBoolean true) @@
        (token @@ (Parsers.string_ @@ string "true")))
    @@ (Parsers.map @@ (constant $ Json.valueBoolean false) @@
        (token @@ (Parsers.string_ @@ string "false")))

-- | Parse a single digit (0-9)
digit :: TBinding (Parser Int)
digit = define "digit" $
  doc "Parse a single digit (0-9)" $
  Parsers.satisfy @@ ("c" ~>
    Logic.and
      (Equality.gte (var "c") zeroCode)
      (Equality.lte (var "c") nineCode))

-- | Parse one or more digits and convert to string
digits :: TBinding (Parser String)
digits = define "digits" $
  doc "Parse one or more digits as a string" $
  Parsers.map @@ (unaryFunction Strings.fromList) @@
    (Parsers.some @@ digit)

-- | Parse the integer part of a JSON number
jsonIntegerPart :: TBinding (Parser String)
jsonIntegerPart = define "jsonIntegerPart" $
  doc "Parse the integer part of a JSON number (optional minus, then digits)" $
  Parsers.bind @@
    (Parsers.optional @@ (Parsers.char @@ minusCode)) @@
    ("sign" ~>
      Parsers.bind @@ digits @@ ("digits" ~>
        Parsers.pure @@
          (Maybes.maybe
            (var "digits")
            (constant $ string "-" ++ var "digits")
            (var "sign"))))

-- | Parse the fractional part of a JSON number
jsonFractionPart :: TBinding (Parser (Maybe String))
jsonFractionPart = define "jsonFractionPart" $
  doc "Parse the optional fractional part of a JSON number" $
  Parsers.optional @@
    (Parsers.bind @@ (Parsers.char @@ dotCode) @@ (constant $
      Parsers.map @@ ("d" ~> string "." ++ var "d") @@ digits))

-- | Parse the exponent part of a JSON number
jsonExponentPart :: TBinding (Parser (Maybe String))
jsonExponentPart = define "jsonExponentPart" $
  doc "Parse the optional exponent part of a JSON number" $
  Parsers.optional @@
    (Parsers.bind @@
      (Parsers.satisfy @@ ("c" ~>
        Logic.or (Equality.equal (var "c") letterELower)
                 (Equality.equal (var "c") letterEUpper))) @@
      (constant $
        Parsers.bind @@
          (Parsers.optional @@
            (Parsers.satisfy @@ ("c" ~>
              Logic.or (Equality.equal (var "c") plusCode)
                       (Equality.equal (var "c") minusCode)))) @@
          ("sign" ~>
            Parsers.map @@
              ("digits" ~>
                string "e" ++
                Maybes.maybe (string "") (unaryFunction Strings.fromList <.> unaryFunction Lists.pure) (var "sign") ++
                var "digits") @@
              digits)))

-- | Parse a JSON number
jsonNumber :: TBinding (Parser J.Value)
jsonNumber = define "jsonNumber" $
  doc "Parse a JSON number (integer, decimal, or scientific notation)" $
  token @@
    (Parsers.bind @@ jsonIntegerPart @@ ("intPart" ~>
      Parsers.bind @@ jsonFractionPart @@ ("fracPart" ~>
        Parsers.bind @@ jsonExponentPart @@ ("expPart" ~>
          "numStr" <~
            (var "intPart" ++
             Maybes.maybe (string "") (unaryFunction Equality.identity) (var "fracPart") ++
             Maybes.maybe (string "") (unaryFunction Equality.identity) (var "expPart")) $
          Parsers.pure @@
            (Json.valueNumber (Maybes.maybe (bigfloat 0.0) (unaryFunction Equality.identity) (Literals.readBigfloat (var "numStr"))))))))

-- | Parse a JSON escape character
jsonEscapeChar :: TBinding (Parser Int)
jsonEscapeChar = define "jsonEscapeChar" $
  doc "Parse a JSON escape sequence after the backslash" $
  Parsers.choice @@ list [
    Parsers.map @@ (constant quoteCode) @@ (Parsers.char @@ quoteCode),
    Parsers.map @@ (constant backslashCode) @@ (Parsers.char @@ backslashCode),
    Parsers.map @@ (constant $ int32 47) @@ (Parsers.char @@ int32 47),  -- '/'
    Parsers.map @@ (constant $ int32 8) @@ (Parsers.char @@ letterBCode),   -- '\b'
    Parsers.map @@ (constant $ int32 12) @@ (Parsers.char @@ letterFCode),  -- '\f'
    Parsers.map @@ (constant newlineCode) @@ (Parsers.char @@ letterNCode), -- '\n'
    Parsers.map @@ (constant returnCode) @@ (Parsers.char @@ letterRCode),  -- '\r'
    Parsers.map @@ (constant tabCode) @@ (Parsers.char @@ letterTCode)]     -- '\t'
    -- Note: \uXXXX unicode escapes not yet implemented

-- | Parse a single JSON string character
jsonStringChar :: TBinding (Parser Int)
jsonStringChar = define "jsonStringChar" $
  doc "Parse a single character in a JSON string (handling escapes)" $
  Parsers.alt @@
    -- Escape sequence
    (Parsers.bind @@ (Parsers.char @@ backslashCode) @@ (constant $
      jsonEscapeChar)) @@
    -- Regular character (not quote or backslash)
    (Parsers.satisfy @@ ("c" ~>
      Logic.and
        (Logic.not $ Equality.equal (var "c") quoteCode)
        (Logic.not $ Equality.equal (var "c") backslashCode)))

-- | Parse a JSON string
jsonString :: TBinding (Parser J.Value)
jsonString = define "jsonString" $
  doc "Parse a JSON string value" $
  token @@
    (Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
      Parsers.bind @@ (Parsers.many @@ jsonStringChar) @@ ("chars" ~>
        Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
          Parsers.pure @@ (Json.valueString (Strings.fromList (var "chars")))))))

-- | Parse a JSON array
jsonArray :: TBinding (Parser J.Value)
jsonArray = define "jsonArray" $
  doc "Parse a JSON array" $
  Parsers.map @@ (unaryFunction Json.valueArray) @@
    (Parsers.between
      @@ (token @@ (Parsers.char @@ bracketOpenCode))
      @@ (token @@ (Parsers.char @@ bracketCloseCode))
      @@ (Parsers.sepBy
          @@ jsonValue
          @@ (token @@ (Parsers.char @@ commaCode))))

-- | Parse a JSON key-value pair
jsonKeyValue :: TBinding (Parser (String, J.Value))
jsonKeyValue = define "jsonKeyValue" $
  doc "Parse a JSON object key-value pair" $
  Parsers.bind @@
    (token @@
      (Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
        Parsers.bind @@ (Parsers.many @@ jsonStringChar) @@ ("chars" ~>
          Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
            Parsers.pure @@ (Strings.fromList (var "chars"))))))) @@
    ("key" ~>
      Parsers.bind @@ (token @@ (Parsers.char @@ colonCode)) @@ (constant $
        Parsers.map @@ ("v" ~> pair (var "key") (var "v")) @@ jsonValue))

-- | Parse a JSON object
jsonObject :: TBinding (Parser J.Value)
jsonObject = define "jsonObject" $
  doc "Parse a JSON object" $
  Parsers.map @@ (unaryFunction Json.valueObject <.> unaryFunction Maps.fromList) @@
    (Parsers.between
      @@ (token @@ (Parsers.char @@ braceOpenCode))
      @@ (token @@ (Parsers.char @@ braceCloseCode))
      @@ (Parsers.sepBy
          @@ jsonKeyValue
          @@ (token @@ (Parsers.char @@ commaCode))))

-- | Parse any JSON value
jsonValue :: TBinding (Parser J.Value)
jsonValue = define "jsonValue" $
  doc "Parse any JSON value" $
  Parsers.choice @@ list [
    jsonNull,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
    jsonObject]

-- | Parse a JSON document (value with optional surrounding whitespace)
parseJson :: TBinding (String -> ParseResult J.Value)
parseJson = define "parseJson" $
  doc "Parse a JSON document from a string" $
  "input" ~>
    Parsing.runParser
      (Parsers.bind @@ whitespace @@ (constant $
        Parsers.bind @@ jsonValue @@ ("v" ~>
          Parsers.bind @@ whitespace @@ (constant $
            Parsers.bind @@ Parsers.eof @@ (constant $
              Parsers.pure @@ var "v")))))
      (var "input")
