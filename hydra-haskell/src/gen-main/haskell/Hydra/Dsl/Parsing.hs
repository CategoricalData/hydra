-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.parsing

module Hydra.Dsl.Parsing where

import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

parseError :: (String -> String -> Parsing.ParseError)
parseError message remainder = Parsing.ParseError {
  Parsing.parseErrorMessage = message,
  Parsing.parseErrorRemainder = remainder}

parseErrorMessage :: (Parsing.ParseError -> String)
parseErrorMessage = Parsing.parseErrorMessage

parseErrorRemainder :: (Parsing.ParseError -> String)
parseErrorRemainder = Parsing.parseErrorRemainder

parseErrorWithMessage :: (Parsing.ParseError -> String -> Parsing.ParseError)
parseErrorWithMessage original newVal = Parsing.ParseError {
  Parsing.parseErrorMessage = newVal,
  Parsing.parseErrorRemainder = (Parsing.parseErrorRemainder original)}

parseErrorWithRemainder :: (Parsing.ParseError -> String -> Parsing.ParseError)
parseErrorWithRemainder original newVal = Parsing.ParseError {
  Parsing.parseErrorMessage = (Parsing.parseErrorMessage original),
  Parsing.parseErrorRemainder = newVal}

parseResultSuccess :: (Parsing.ParseSuccess t0 -> Parsing.ParseResult t0)
parseResultSuccess x = (Parsing.ParseResultSuccess x)

parseResultFailure :: (Parsing.ParseError -> Parsing.ParseResult t0)
parseResultFailure x = (Parsing.ParseResultFailure x)

parseSuccess :: (t0 -> String -> Parsing.ParseSuccess t0)
parseSuccess value remainder = Parsing.ParseSuccess {
  Parsing.parseSuccessValue = value,
  Parsing.parseSuccessRemainder = remainder}

parseSuccessValue :: (Parsing.ParseSuccess t0 -> t0)
parseSuccessValue = Parsing.parseSuccessValue

parseSuccessRemainder :: (Parsing.ParseSuccess t0 -> String)
parseSuccessRemainder = Parsing.parseSuccessRemainder

parseSuccessWithValue :: (Parsing.ParseSuccess t0 -> t1 -> Parsing.ParseSuccess t1)
parseSuccessWithValue original newVal = Parsing.ParseSuccess {
  Parsing.parseSuccessValue = newVal,
  Parsing.parseSuccessRemainder = (Parsing.parseSuccessRemainder original)}

parseSuccessWithRemainder :: (Parsing.ParseSuccess t0 -> String -> Parsing.ParseSuccess t0)
parseSuccessWithRemainder original newVal = Parsing.ParseSuccess {
  Parsing.parseSuccessValue = (Parsing.parseSuccessValue original),
  Parsing.parseSuccessRemainder = newVal}

parser :: ((String -> Parsing.ParseResult t0) -> Parsing.Parser t0)
parser x = (Parsing.Parser x)

unParser :: (Parsing.Parser t0 -> String -> Parsing.ParseResult t0)
unParser = Parsing.unParser
