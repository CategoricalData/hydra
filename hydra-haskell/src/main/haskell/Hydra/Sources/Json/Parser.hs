{-# LANGUAGE OverloadedStrings #-}

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
import qualified Hydra.Dsl.Meta.Testing       as Testing
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

import qualified Hydra.Json as J


jsonParserDefinition :: String -> TTerm a -> TBinding a
jsonParserDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [Parsers.module_]
    KernelTypes.kernelTypesModules $
    Just "JSON parser using Hydra parser combinators"
  where
    ns = Namespace "hydra.json.parser"
    elements = [
      el whitespaceDef,
      el tokenDef,
      el jsonNullDef,
      el jsonBoolDef,
      el digitDef,
      el digitsDef,
      el jsonIntegerPartDef,
      el jsonFractionPartDef,
      el jsonExponentPartDef,
      el jsonNumberDef,
      el jsonEscapeCharDef,
      el jsonStringCharDef,
      el jsonStringDef,
      el jsonArrayDef,
      el jsonKeyValueDef,
      el jsonObjectDef,
      el jsonValueDef,
      el parseJsonDef]

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
whitespaceDef :: TBinding (Parser ())
whitespaceDef = define "whitespace" $
  doc "Parse zero or more JSON whitespace characters (space, tab, newline, carriage return)" $
  ref Parsers.mapDef @@ (constant unit) @@
    (ref Parsers.manyDef @@
      (ref Parsers.satisfyDef @@ ("c" ~>
        Logic.ors (list [
          Equality.equal (var "c") spaceCode,
          Equality.equal (var "c") tabCode,
          Equality.equal (var "c") newlineCode,
          Equality.equal (var "c") returnCode]))))

-- | Parse a token followed by optional whitespace
tokenDef :: TBinding (Parser a -> Parser a)
tokenDef = define "token" $
  doc "Parse a token followed by optional whitespace" $
  "p" ~>
    ref Parsers.bindDef @@ var "p" @@ ("x" ~>
      ref Parsers.bindDef @@ ref whitespaceDef @@ (constant $
        ref Parsers.pureDef @@ var "x"))

-- | Parse JSON null
jsonNullDef :: TBinding (Parser J.Value)
jsonNullDef = define "jsonNull" $
  doc "Parse JSON null value" $
  ref Parsers.mapDef @@ (constant Json.valueNull) @@
    (ref tokenDef @@ (ref Parsers.stringDef @@ string "null"))

-- | Parse JSON boolean
jsonBoolDef :: TBinding (Parser J.Value)
jsonBoolDef = define "jsonBool" $
  doc "Parse JSON boolean (true or false)" $
  ref Parsers.altDef
    @@ (ref Parsers.mapDef @@ (constant $ Json.valueBoolean true) @@
        (ref tokenDef @@ (ref Parsers.stringDef @@ string "true")))
    @@ (ref Parsers.mapDef @@ (constant $ Json.valueBoolean false) @@
        (ref tokenDef @@ (ref Parsers.stringDef @@ string "false")))

-- | Parse a single digit (0-9)
digitDef :: TBinding (Parser Int)
digitDef = define "digit" $
  doc "Parse a single digit (0-9)" $
  ref Parsers.satisfyDef @@ ("c" ~>
    Logic.and
      (Equality.gte (var "c") zeroCode)
      (Equality.lte (var "c") nineCode))

-- | Parse one or more digits and convert to string
digitsDef :: TBinding (Parser String)
digitsDef = define "digits" $
  doc "Parse one or more digits as a string" $
  ref Parsers.mapDef @@ (unaryFunction Strings.fromList) @@
    (ref Parsers.someDef @@ ref digitDef)

-- | Parse the integer part of a JSON number
jsonIntegerPartDef :: TBinding (Parser String)
jsonIntegerPartDef = define "jsonIntegerPart" $
  doc "Parse the integer part of a JSON number (optional minus, then digits)" $
  ref Parsers.bindDef @@
    (ref Parsers.optionalDef @@ (ref Parsers.charDef @@ minusCode)) @@
    ("sign" ~>
      ref Parsers.bindDef @@ ref digitsDef @@ ("digits" ~>
        ref Parsers.pureDef @@
          (Maybes.maybe
            (var "digits")
            (constant $ string "-" ++ var "digits")
            (var "sign"))))

-- | Parse the fractional part of a JSON number
jsonFractionPartDef :: TBinding (Parser (Maybe String))
jsonFractionPartDef = define "jsonFractionPart" $
  doc "Parse the optional fractional part of a JSON number" $
  ref Parsers.optionalDef @@
    (ref Parsers.bindDef @@ (ref Parsers.charDef @@ dotCode) @@ (constant $
      ref Parsers.mapDef @@ ("d" ~> string "." ++ var "d") @@ ref digitsDef))

-- | Parse the exponent part of a JSON number
jsonExponentPartDef :: TBinding (Parser (Maybe String))
jsonExponentPartDef = define "jsonExponentPart" $
  doc "Parse the optional exponent part of a JSON number" $
  ref Parsers.optionalDef @@
    (ref Parsers.bindDef @@
      (ref Parsers.satisfyDef @@ ("c" ~>
        Logic.or (Equality.equal (var "c") letterELower)
                 (Equality.equal (var "c") letterEUpper))) @@
      (constant $
        ref Parsers.bindDef @@
          (ref Parsers.optionalDef @@
            (ref Parsers.satisfyDef @@ ("c" ~>
              Logic.or (Equality.equal (var "c") plusCode)
                       (Equality.equal (var "c") minusCode)))) @@
          ("sign" ~>
            ref Parsers.mapDef @@
              ("digits" ~>
                string "e" ++
                Maybes.maybe (string "") (unaryFunction Strings.fromList <.> unaryFunction Lists.pure) (var "sign") ++
                var "digits") @@
              ref digitsDef)))

-- | Parse a JSON number
jsonNumberDef :: TBinding (Parser J.Value)
jsonNumberDef = define "jsonNumber" $
  doc "Parse a JSON number (integer, decimal, or scientific notation)" $
  ref tokenDef @@
    (ref Parsers.bindDef @@ ref jsonIntegerPartDef @@ ("intPart" ~>
      ref Parsers.bindDef @@ ref jsonFractionPartDef @@ ("fracPart" ~>
        ref Parsers.bindDef @@ ref jsonExponentPartDef @@ ("expPart" ~>
          "numStr" <~
            (var "intPart" ++
             Maybes.maybe (string "") (unaryFunction Equality.identity) (var "fracPart") ++
             Maybes.maybe (string "") (unaryFunction Equality.identity) (var "expPart")) $
          ref Parsers.pureDef @@
            (Json.valueNumber (Maybes.maybe (bigfloat 0.0) (unaryFunction Equality.identity) (Literals.readBigfloat (var "numStr"))))))))

-- | Parse a JSON escape character
jsonEscapeCharDef :: TBinding (Parser Int)
jsonEscapeCharDef = define "jsonEscapeChar" $
  doc "Parse a JSON escape sequence after the backslash" $
  ref Parsers.choiceDef @@ list [
    ref Parsers.mapDef @@ (constant quoteCode) @@ (ref Parsers.charDef @@ quoteCode),
    ref Parsers.mapDef @@ (constant backslashCode) @@ (ref Parsers.charDef @@ backslashCode),
    ref Parsers.mapDef @@ (constant $ int32 47) @@ (ref Parsers.charDef @@ int32 47),  -- '/'
    ref Parsers.mapDef @@ (constant $ int32 8) @@ (ref Parsers.charDef @@ letterBCode),   -- '\b'
    ref Parsers.mapDef @@ (constant $ int32 12) @@ (ref Parsers.charDef @@ letterFCode),  -- '\f'
    ref Parsers.mapDef @@ (constant newlineCode) @@ (ref Parsers.charDef @@ letterNCode), -- '\n'
    ref Parsers.mapDef @@ (constant returnCode) @@ (ref Parsers.charDef @@ letterRCode),  -- '\r'
    ref Parsers.mapDef @@ (constant tabCode) @@ (ref Parsers.charDef @@ letterTCode)]     -- '\t'
    -- Note: \uXXXX unicode escapes not yet implemented

-- | Parse a single JSON string character
jsonStringCharDef :: TBinding (Parser Int)
jsonStringCharDef = define "jsonStringChar" $
  doc "Parse a single character in a JSON string (handling escapes)" $
  ref Parsers.altDef @@
    -- Escape sequence
    (ref Parsers.bindDef @@ (ref Parsers.charDef @@ backslashCode) @@ (constant $
      ref jsonEscapeCharDef)) @@
    -- Regular character (not quote or backslash)
    (ref Parsers.satisfyDef @@ ("c" ~>
      Logic.and
        (Logic.not $ Equality.equal (var "c") quoteCode)
        (Logic.not $ Equality.equal (var "c") backslashCode)))

-- | Parse a JSON string
jsonStringDef :: TBinding (Parser J.Value)
jsonStringDef = define "jsonString" $
  doc "Parse a JSON string value" $
  ref tokenDef @@
    (ref Parsers.bindDef @@ (ref Parsers.charDef @@ quoteCode) @@ (constant $
      ref Parsers.bindDef @@ (ref Parsers.manyDef @@ ref jsonStringCharDef) @@ ("chars" ~>
        ref Parsers.bindDef @@ (ref Parsers.charDef @@ quoteCode) @@ (constant $
          ref Parsers.pureDef @@ (Json.valueString (Strings.fromList (var "chars")))))))

-- | Parse a JSON array
jsonArrayDef :: TBinding (Parser J.Value)
jsonArrayDef = define "jsonArray" $
  doc "Parse a JSON array" $
  ref Parsers.mapDef @@ (unaryFunction Json.valueArray) @@
    (ref Parsers.betweenDef
      @@ (ref tokenDef @@ (ref Parsers.charDef @@ bracketOpenCode))
      @@ (ref tokenDef @@ (ref Parsers.charDef @@ bracketCloseCode))
      @@ (ref Parsers.sepByDef
          @@ ref jsonValueDef
          @@ (ref tokenDef @@ (ref Parsers.charDef @@ commaCode))))

-- | Parse a JSON key-value pair
jsonKeyValueDef :: TBinding (Parser (String, J.Value))
jsonKeyValueDef = define "jsonKeyValue" $
  doc "Parse a JSON object key-value pair" $
  ref Parsers.bindDef @@
    (ref tokenDef @@
      (ref Parsers.bindDef @@ (ref Parsers.charDef @@ quoteCode) @@ (constant $
        ref Parsers.bindDef @@ (ref Parsers.manyDef @@ ref jsonStringCharDef) @@ ("chars" ~>
          ref Parsers.bindDef @@ (ref Parsers.charDef @@ quoteCode) @@ (constant $
            ref Parsers.pureDef @@ (Strings.fromList (var "chars"))))))) @@
    ("key" ~>
      ref Parsers.bindDef @@ (ref tokenDef @@ (ref Parsers.charDef @@ colonCode)) @@ (constant $
        ref Parsers.mapDef @@ ("v" ~> pair (var "key") (var "v")) @@ ref jsonValueDef))

-- | Parse a JSON object
jsonObjectDef :: TBinding (Parser J.Value)
jsonObjectDef = define "jsonObject" $
  doc "Parse a JSON object" $
  ref Parsers.mapDef @@ (unaryFunction Json.valueObject <.> unaryFunction Maps.fromList) @@
    (ref Parsers.betweenDef
      @@ (ref tokenDef @@ (ref Parsers.charDef @@ braceOpenCode))
      @@ (ref tokenDef @@ (ref Parsers.charDef @@ braceCloseCode))
      @@ (ref Parsers.sepByDef
          @@ ref jsonKeyValueDef
          @@ (ref tokenDef @@ (ref Parsers.charDef @@ commaCode))))

-- | Parse any JSON value
jsonValueDef :: TBinding (Parser J.Value)
jsonValueDef = define "jsonValue" $
  doc "Parse any JSON value" $
  ref Parsers.choiceDef @@ list [
    ref jsonNullDef,
    ref jsonBoolDef,
    ref jsonNumberDef,
    ref jsonStringDef,
    ref jsonArrayDef,
    ref jsonObjectDef]

-- | Parse a JSON document (value with optional surrounding whitespace)
parseJsonDef :: TBinding (String -> ParseResult J.Value)
parseJsonDef = define "parseJson" $
  doc "Parse a JSON document from a string" $
  "input" ~>
    Parsing.runParser
      (ref Parsers.bindDef @@ ref whitespaceDef @@ (constant $
        ref Parsers.bindDef @@ ref jsonValueDef @@ ("v" ~>
          ref Parsers.bindDef @@ ref whitespaceDef @@ (constant $
            ref Parsers.bindDef @@ ref Parsers.eofDef @@ (constant $
              ref Parsers.pureDef @@ var "v")))))
      (var "input")
