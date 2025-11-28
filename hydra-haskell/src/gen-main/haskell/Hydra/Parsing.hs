-- Note: this is an automatically generated file. Do not edit.

-- | Parser combinator types for text parsing

module Hydra.Parsing where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An error which occurred while parsing
data ParseError = 
  ParseError {
    -- | An error message
    parseErrorMessage :: String,
    -- | The remaining input at the point of failure
    parseErrorRemainder :: String}
  deriving (Eq, Ord, Read, Show)

_ParseError = (Core.Name "hydra.parsing.ParseError")

_ParseError_message = (Core.Name "message")

_ParseError_remainder = (Core.Name "remainder")

-- | The result of a parse operation
data ParseResult a = 
  -- | A successful parse, with a value and the remaining unparsed input
  ParseResultSuccess (ParseSuccess a) |
  -- | A failed parse, with an error message and the remaining input
  ParseResultFailure ParseError
  deriving (Eq, Ord, Read, Show)

_ParseResult = (Core.Name "hydra.parsing.ParseResult")

_ParseResult_success = (Core.Name "success")

_ParseResult_failure = (Core.Name "failure")

-- | A successful parse result
data ParseSuccess a = 
  ParseSuccess {
    -- | The parsed value
    parseSuccessValue :: a,
    -- | The remaining unparsed input
    parseSuccessRemainder :: String}
  deriving (Eq, Ord, Read, Show)

_ParseSuccess = (Core.Name "hydra.parsing.ParseSuccess")

_ParseSuccess_value = (Core.Name "value")

_ParseSuccess_remainder = (Core.Name "remainder")

-- | A parser which consumes characters from a string and produces a value
newtype Parser a = 
  Parser {
    unParser :: (String -> ParseResult a)}

_Parser = (Core.Name "hydra.parsing.Parser")
