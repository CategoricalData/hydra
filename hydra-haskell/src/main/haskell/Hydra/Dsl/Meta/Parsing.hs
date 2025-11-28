-- | Phantom-typed term DSL for the hydra.parsing types

module Hydra.Dsl.Meta.Parsing where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Data.Map as M


-- | Construct a ParseError
parseError :: TTerm String -> TTerm String -> TTerm ParseError
parseError message remainder = record _ParseError [
  _ParseError_message>>: message,
  _ParseError_remainder>>: remainder]

-- | Get the message from a ParseError
parseErrorMessage :: TTerm ParseError -> TTerm String
parseErrorMessage e = project _ParseError _ParseError_message @@ e

-- | Get the remainder from a ParseError
parseErrorRemainder :: TTerm ParseError -> TTerm String
parseErrorRemainder e = project _ParseError _ParseError_remainder @@ e

-- | Construct a successful ParseResult
parseResultSuccess :: TTerm (ParseSuccess a) -> TTerm (ParseResult a)
parseResultSuccess = inject _ParseResult _ParseResult_success

-- | Construct a failed ParseResult
parseResultFailure :: TTerm ParseError -> TTerm (ParseResult a)
parseResultFailure = inject _ParseResult _ParseResult_failure

-- | Construct a ParseSuccess
parseSuccess :: TTerm a -> TTerm String -> TTerm (ParseSuccess a)
parseSuccess value remainder = record _ParseSuccess [
  _ParseSuccess_value>>: value,
  _ParseSuccess_remainder>>: remainder]

-- | Get the value from a ParseSuccess
parseSuccessValue :: TTerm (ParseSuccess a) -> TTerm a
parseSuccessValue s = project _ParseSuccess _ParseSuccess_value @@ s

-- | Get the remainder from a ParseSuccess
parseSuccessRemainder :: TTerm (ParseSuccess a) -> TTerm String
parseSuccessRemainder s = project _ParseSuccess _ParseSuccess_remainder @@ s

-- | Wrap a function as a Parser
parser :: TTerm (String -> ParseResult a) -> TTerm (Parser a)
parser = wrap _Parser

-- | Unwrap a Parser to get its function
runParser :: TTerm (Parser a) -> TTerm String -> TTerm (ParseResult a)
runParser p input = unwrap _Parser @@ p @@ input
