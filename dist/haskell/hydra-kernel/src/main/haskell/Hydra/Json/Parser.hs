-- Note: this is an automatically generated file. Do not edit.
-- | JSON parser using Hydra parser combinators

module Hydra.Json.Parser where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsers as Parsers
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Hydra.Haskell.Lib.Literals as Literals
-- | Parse a single digit (0-9)
digit :: Parsing.Parser Int
digit = Parsers.satisfy (\c -> Logic.and (Equality.gte c 48) (Equality.lte c 57))
-- | Parse one or more digits as a string
digits :: Parsing.Parser String
digits = Parsers.map Strings.fromList (Parsers.some digit)
-- | Parse a JSON array
jsonArray :: Parsing.Parser Model.Value
jsonArray =
    Parsers.map (\x -> Model.ValueArray x) (Parsers.between (token (Parsers.char 91)) (token (Parsers.char 93)) (Parsers.sepBy (Parsers.lazy (\_ -> jsonValue)) (token (Parsers.char 44))))
-- | Parse JSON boolean (true or false)
jsonBool :: Parsing.Parser Model.Value
jsonBool =
    Parsers.alt (Parsers.map (\_ -> Model.ValueBoolean True) (token (Parsers.string "true"))) (Parsers.map (\_ -> Model.ValueBoolean False) (token (Parsers.string "false")))
-- | Parse a JSON escape sequence after the backslash
jsonEscapeChar :: Parsing.Parser Int
jsonEscapeChar =
    Parsers.choice [
      Parsers.map (\_ -> 34) (Parsers.char 34),
      (Parsers.map (\_ -> 92) (Parsers.char 92)),
      (Parsers.map (\_ -> 47) (Parsers.char 47)),
      (Parsers.map (\_ -> 8) (Parsers.char 98)),
      (Parsers.map (\_ -> 12) (Parsers.char 102)),
      (Parsers.map (\_ -> 10) (Parsers.char 110)),
      (Parsers.map (\_ -> 13) (Parsers.char 114)),
      (Parsers.map (\_ -> 9) (Parsers.char 116))]
-- | Parse the optional exponent part of a JSON number
jsonExponentPart :: Parsing.Parser (Maybe String)
jsonExponentPart =
    Parsers.optional (Parsers.bind (Parsers.satisfy (\c -> Logic.or (Equality.equal c 101) (Equality.equal c 69))) (\_ -> Parsers.bind (Parsers.optional (Parsers.satisfy (\c -> Logic.or (Equality.equal c 43) (Equality.equal c 45)))) (\sign -> Parsers.map (\digits -> Strings.cat2 (Strings.cat2 "e" (Optionals.cases sign "" (\arg_ -> Strings.fromList (Lists.pure arg_)))) digits) digits)))
-- | Parse the optional fractional part of a JSON number
jsonFractionPart :: Parsing.Parser (Maybe String)
jsonFractionPart = Parsers.optional (Parsers.bind (Parsers.char 46) (\_ -> Parsers.map (\d -> Strings.cat2 "." d) digits))
-- | Parse the integer part of a JSON number (optional minus, then digits)
jsonIntegerPart :: Parsing.Parser String
jsonIntegerPart =
    Parsers.bind (Parsers.optional (Parsers.char 45)) (\sign -> Parsers.bind digits (\digits -> Parsers.pure (Optionals.cases sign digits (\_ -> Strings.cat2 "-" digits))))
-- | Parse a JSON object key-value pair
jsonKeyValue :: Parsing.Parser (String, Model.Value)
jsonKeyValue =
    Parsers.bind (token (Parsers.bind (Parsers.char 34) (\_ -> Parsers.bind (Parsers.many jsonStringChar) (\chars -> Parsers.bind (Parsers.char 34) (\_2 -> Parsers.pure (Strings.fromList chars)))))) (\key -> Parsers.bind (token (Parsers.char 58)) (\_ -> Parsers.map (\v -> (key, v)) (Parsers.lazy (\_2 -> jsonValue))))
-- | Parse JSON null value
jsonNull :: Parsing.Parser Model.Value
jsonNull = Parsers.map (\_ -> Model.ValueNull) (token (Parsers.string "null"))
-- | Parse a JSON number (integer, decimal, or scientific notation)
jsonNumber :: Parsing.Parser Model.Value
jsonNumber =
    token (Parsers.bind jsonIntegerPart (\intPart -> Parsers.bind jsonFractionPart (\fracPart -> Parsers.bind jsonExponentPart (\expPart ->
      let numStr =
              Strings.cat2 (Strings.cat2 intPart (Optionals.cases fracPart "" Equality.identity)) (Optionals.cases expPart "" Equality.identity)
      in (Parsers.pure (Model.ValueNumber (Optionals.cases (Literals.readDecimal numStr) (Literals.stringToDecimal "0.0") Equality.identity)))))))
-- | Parse a JSON object
jsonObject :: Parsing.Parser Model.Value
jsonObject =
    Parsers.map (\arg_ -> (\x -> Model.ValueObject x) (Maps.fromList arg_)) (Parsers.between (token (Parsers.char 123)) (token (Parsers.char 125)) (Parsers.sepBy jsonKeyValue (token (Parsers.char 44))))
-- | Parse a JSON string value
jsonString :: Parsing.Parser Model.Value
jsonString =
    token (Parsers.bind (Parsers.char 34) (\_ -> Parsers.bind (Parsers.many jsonStringChar) (\chars -> Parsers.bind (Parsers.char 34) (\_2 -> Parsers.pure (Model.ValueString (Strings.fromList chars))))))
-- | Parse a single character in a JSON string (handling escapes)
jsonStringChar :: Parsing.Parser Int
jsonStringChar =
    Parsers.alt (Parsers.bind (Parsers.char 92) (\_ -> jsonEscapeChar)) (Parsers.satisfy (\c -> Logic.and (Logic.not (Equality.equal c 34)) (Logic.not (Equality.equal c 92))))
-- | Parse any JSON value
jsonValue :: Parsing.Parser Model.Value
jsonValue =
    Parsers.choice [
      jsonNull,
      jsonBool,
      jsonNumber,
      jsonString,
      jsonArray,
      jsonObject]
-- | Parse a JSON document from a string
parseJson :: String -> Parsing.ParseResult Model.Value
parseJson input =
    Parsing.unParser (Parsers.bind whitespace (\_ -> Parsers.bind jsonValue (\v -> Parsers.bind whitespace (\_2 -> Parsers.bind Parsers.eof (\_3 -> Parsers.pure v))))) input
-- | Parse a token followed by optional whitespace
token :: Parsing.Parser t0 -> Parsing.Parser t0
token p = Parsers.bind p (\x -> Parsers.bind whitespace (\_ -> Parsers.pure x))
-- | Parse zero or more JSON whitespace characters (space, tab, newline, carriage return)
whitespace :: Parsing.Parser ()
whitespace =
    Parsers.map (\_ -> ()) (Parsers.many (Parsers.satisfy (\c -> Lists.foldl Logic.or False [
      Equality.equal c 32,
      (Equality.equal c 9),
      (Equality.equal c 10),
      (Equality.equal c 13)])))
