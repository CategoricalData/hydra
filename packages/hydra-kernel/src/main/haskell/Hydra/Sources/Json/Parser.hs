
module Hydra.Sources.Json.Parser where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants      as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Dsl.Parsing                    as Parsing
import qualified Hydra.Sources.Kernel.Terms.Parsers        as Parsers
import qualified Hydra.Json.Model as J


module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Parsers.ns] L.++ KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "JSON parser using Hydra parser combinators")}
  where
    ns = ModuleName "hydra.json.parser"
    definitions = [
      toDefinition digit,
      toDefinition digits,
      toDefinition jsonArray,
      toDefinition jsonBool,
      toDefinition jsonEscapeChar,
      toDefinition jsonExponentPart,
      toDefinition jsonFractionPart,
      toDefinition jsonIntegerPart,
      toDefinition jsonKeyValue,
      toDefinition jsonNull,
      toDefinition jsonNumber,
      toDefinition jsonObject,
      toDefinition jsonString,
      toDefinition jsonStringChar,
      toDefinition jsonValue,
      toDefinition parseJson,
      toDefinition token,
      toDefinition whitespace]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | ASCII code points for common characters
spaceCode, tabCode, newlineCode, returnCode :: TypedTerm Int
spaceCode = int32 32      -- ' '
tabCode = int32 9         -- '\t'
newlineCode = int32 10    -- '\n'
returnCode = int32 13     -- '\r'

quoteCode, backslashCode, colonCode, commaCode :: TypedTerm Int
quoteCode = int32 34      -- '"'
backslashCode = int32 92  -- '\\'
colonCode = int32 58      -- ':'
commaCode = int32 44      -- ','

bracketOpenCode, bracketCloseCode, braceOpenCode, braceCloseCode :: TypedTerm Int
bracketOpenCode = int32 91   -- '['
bracketCloseCode = int32 93  -- ']'
braceOpenCode = int32 123    -- '{'
braceCloseCode = int32 125   -- '}'

zeroCode, nineCode, minusCode, plusCode, dotCode :: TypedTerm Int
zeroCode = int32 48       -- '0'
nineCode = int32 57       -- '9'
minusCode = int32 45      -- '-'
plusCode = int32 43       -- '+'
dotCode = int32 46        -- '.'

letterELower, letterEUpper :: TypedTerm Int
letterELower = int32 101  -- 'e'
letterEUpper = int32 69   -- 'E'

letterNCode, letterUCode, letterLCode :: TypedTerm Int
letterNCode = int32 110   -- 'n'
letterUCode = int32 117   -- 'u'
letterLCode = int32 108   -- 'l'

letterTCode, letterRCode, letterFCode, letterACode, letterSCode :: TypedTerm Int
letterTCode = int32 116   -- 't'
letterRCode = int32 114   -- 'r'
letterFCode = int32 102   -- 'f'
letterACode = int32 97    -- 'a'
letterSCode = int32 115   -- 's'

-- | Parse a single digit (0-9)
digit :: TypedTermDefinition (Parser Int)
digit = define "digit" $
  doc "Parse a single digit (0-9)" $
  Parsers.satisfy @@ ("c" ~>
    Logic.and
      (Equality.gte (var "c") zeroCode)
      (Equality.lte (var "c") nineCode))

-- | Parse one or more digits and convert to string
-- | Parse one or more digits and convert to string
digits :: TypedTermDefinition (Parser String)
digits = define "digits" $
  doc "Parse one or more digits as a string" $
  Parsers.map @@ (reify Strings.fromList) @@
    (Parsers.some @@ digit)

-- | Parse the integer part of a JSON number
-- | Parse a JSON array
jsonArray :: TypedTermDefinition (Parser J.Value)
jsonArray = define "jsonArray" $
  doc "Parse a JSON array" $
  Parsers.map @@ (reify Json.valueArray) @@
    (Parsers.between
      @@ (token @@ (Parsers.char @@ bracketOpenCode))
      @@ (token @@ (Parsers.char @@ bracketCloseCode))
      @@ (Parsers.sepBy
          @@ (Parsers.lazy @@ constant jsonValue)
          @@ (token @@ (Parsers.char @@ commaCode))))

-- | Parse a JSON key-value pair
-- | Parse JSON boolean
jsonBool :: TypedTermDefinition (Parser J.Value)
jsonBool = define "jsonBool" $
  doc "Parse JSON boolean (true or false)" $
  Parsers.alt
    @@ (Parsers.map @@ (constant $ Json.valueBoolean true) @@
        (token @@ (Parsers.string_ @@ string "true")))
    @@ (Parsers.map @@ (constant $ Json.valueBoolean false) @@
        (token @@ (Parsers.string_ @@ string "false")))

-- | Parse a single digit (0-9)
-- | Parse a JSON escape character
jsonEscapeChar :: TypedTermDefinition (Parser Int)
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
-- | Parse the exponent part of a JSON number
jsonExponentPart :: TypedTermDefinition (Parser (Maybe String))
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
                Maybes.maybe (string "") (reify Strings.fromList <.> reify Lists.pure) (var "sign") ++
                var "digits") @@
              digits)))

-- | Parse a JSON number
-- | Parse the fractional part of a JSON number
jsonFractionPart :: TypedTermDefinition (Parser (Maybe String))
jsonFractionPart = define "jsonFractionPart" $
  doc "Parse the optional fractional part of a JSON number" $
  Parsers.optional @@
    (Parsers.bind @@ (Parsers.char @@ dotCode) @@ (constant $
      Parsers.map @@ ("d" ~> string "." ++ var "d") @@ digits))

-- | Parse the exponent part of a JSON number
-- | Parse the integer part of a JSON number
jsonIntegerPart :: TypedTermDefinition (Parser String)
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
-- | Parse a JSON key-value pair
jsonKeyValue :: TypedTermDefinition (Parser (String, J.Value))
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
        Parsers.map @@ ("v" ~> pair (var "key") (var "v")) @@ (Parsers.lazy @@ constant jsonValue)))

-- | Parse a JSON object
-- | Parse JSON null
jsonNull :: TypedTermDefinition (Parser J.Value)
jsonNull = define "jsonNull" $
  doc "Parse JSON null value" $
  Parsers.map @@ (constant Json.valueNull) @@
    (token @@ (Parsers.string_ @@ string "null"))

-- | Parse JSON boolean
-- | Parse a JSON number
jsonNumber :: TypedTermDefinition (Parser J.Value)
jsonNumber = define "jsonNumber" $
  doc "Parse a JSON number (integer, decimal, or scientific notation)" $
  token @@
    (Parsers.bind @@ jsonIntegerPart @@ ("intPart" ~>
      Parsers.bind @@ jsonFractionPart @@ ("fracPart" ~>
        Parsers.bind @@ jsonExponentPart @@ ("expPart" ~>
          "numStr" <~
            (var "intPart" ++
             Maybes.maybe (string "") (reify Equality.identity) (var "fracPart") ++
             Maybes.maybe (string "") (reify Equality.identity) (var "expPart")) $
          Parsers.pure @@
            (Json.valueNumber (Maybes.maybe (decimal 0) (reify Equality.identity) (Literals.readDecimal (var "numStr"))))))))

-- | Parse a JSON escape character
-- | Parse a JSON object
jsonObject :: TypedTermDefinition (Parser J.Value)
jsonObject = define "jsonObject" $
  doc "Parse a JSON object" $
  Parsers.map @@ (reify Json.valueObject <.> reify Maps.fromList) @@
    (Parsers.between
      @@ (token @@ (Parsers.char @@ braceOpenCode))
      @@ (token @@ (Parsers.char @@ braceCloseCode))
      @@ (Parsers.sepBy
          @@ jsonKeyValue
          @@ (token @@ (Parsers.char @@ commaCode))))

jsonParserDefinition :: String -> TypedTerm a -> TypedTermDefinition a
jsonParserDefinition = definitionInModule module_

-- | Parse any JSON value
-- | Parse a JSON string
jsonString :: TypedTermDefinition (Parser J.Value)
jsonString = define "jsonString" $
  doc "Parse a JSON string value" $
  token @@
    (Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
      Parsers.bind @@ (Parsers.many @@ jsonStringChar) @@ ("chars" ~>
        Parsers.bind @@ (Parsers.char @@ quoteCode) @@ (constant $
          Parsers.pure @@ (Json.valueString (Strings.fromList (var "chars")))))))

-- | Parse a JSON array
-- | Parse a single JSON string character
jsonStringChar :: TypedTermDefinition (Parser Int)
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
-- | Parse any JSON value
jsonValue :: TypedTermDefinition (Parser J.Value)
jsonValue = define "jsonValue" $
  doc "Parse any JSON value" $
  Parsers.choice @@ list [
    jsonNull,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonArray,
    jsonObject]

letterBCode :: TypedTerm Int
letterBCode = int32 98    -- 'b'

-- | Parse a JSON document (value with optional surrounding whitespace)
-- | Parse a JSON document (value with optional surrounding whitespace)
parseJson :: TypedTermDefinition (String -> ParseResult J.Value)
parseJson = define "parseJson" $
  doc "Parse a JSON document from a string" $
  "input" ~>
    unwrap _Parser @@
      (Parsers.bind @@ whitespace @@ (constant $
        Parsers.bind @@ jsonValue @@ ("v" ~>
          Parsers.bind @@ whitespace @@ (constant $
            Parsers.bind @@ Parsers.eof @@ (constant $
              Parsers.pure @@ var "v"))))) @@
      (var "input")
-- | Parse a token followed by optional whitespace
token :: TypedTermDefinition (Parser a -> Parser a)
token = define "token" $
  doc "Parse a token followed by optional whitespace" $
  "p" ~>
    Parsers.bind @@ var "p" @@ ("x" ~>
      Parsers.bind @@ whitespace @@ (constant $
        Parsers.pure @@ var "x"))

-- | Parse JSON null
-- | Parse zero or more whitespace characters
whitespace :: TypedTermDefinition (Parser ())
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
