-- Note: this is an automatically generated file. Do not edit.

-- | General-purpose parser combinators

module Hydra.Parsers where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alt :: (Parsing.Parser t0 -> Parsing.Parser t0 -> Parsing.Parser t0)
alt p1 p2 =  
  let parse = (\input -> (\x -> case x of
          Parsing.ParseResultSuccess v1 -> (Parsing.ParseResultSuccess v1)
          Parsing.ParseResultFailure v1 -> (Logic.ifElse (Equality.equal (Parsing.parseErrorRemainder v1) input) (Parsing.unParser p2 input) (Parsing.ParseResultFailure v1))) (Parsing.unParser p1 input))
  in (Parsing.Parser parse)

-- | Parse any single character (codepoint)
anyChar :: (Parsing.Parser Int)
anyChar = (satisfy (\_ -> True))

apply :: (Parsing.Parser (t0 -> t1) -> Parsing.Parser t0 -> Parsing.Parser t1)
apply pf pa =  
  let parse = (\input -> (\x -> case x of
          Parsing.ParseResultSuccess v1 -> ((\x -> case x of
            Parsing.ParseResultSuccess v2 -> (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
              Parsing.parseSuccessValue = (Parsing.parseSuccessValue v1 (Parsing.parseSuccessValue v2)),
              Parsing.parseSuccessRemainder = (Parsing.parseSuccessRemainder v2)}))
            Parsing.ParseResultFailure v2 -> (Parsing.ParseResultFailure v2)) (Parsing.unParser pa (Parsing.parseSuccessRemainder v1)))
          Parsing.ParseResultFailure v1 -> (Parsing.ParseResultFailure v1)) (Parsing.unParser pf input))
  in (Parsing.Parser parse)

between :: (Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser t2 -> Parsing.Parser t2)
between open close p = (bind open (\_ -> bind p (\x -> bind close (\_ -> pure x))))

bind :: (Parsing.Parser t0 -> (t0 -> Parsing.Parser t1) -> Parsing.Parser t1)
bind pa f =  
  let parse = (\input -> (\x -> case x of
          Parsing.ParseResultSuccess v1 -> (Parsing.unParser (f (Parsing.parseSuccessValue v1)) (Parsing.parseSuccessRemainder v1))
          Parsing.ParseResultFailure v1 -> (Parsing.ParseResultFailure v1)) (Parsing.unParser pa input))
  in (Parsing.Parser parse)

-- | Parse a specific character (codepoint)
char :: (Int -> Parsing.Parser Int)
char c = (satisfy (\x -> Equality.equal x c))

choice :: ([Parsing.Parser t0] -> Parsing.Parser t0)
choice ps = (Lists.foldl alt (fail "no choice matched") ps)

-- | A parser that succeeds only at the end of input
eof :: (Parsing.Parser ())
eof = (Parsing.Parser (\input -> Logic.ifElse (Equality.equal input "") (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
  Parsing.parseSuccessValue = (),
  Parsing.parseSuccessRemainder = ""})) (Parsing.ParseResultFailure (Parsing.ParseError {
  Parsing.parseErrorMessage = "expected end of input",
  Parsing.parseErrorRemainder = input}))))

fail :: (String -> Parsing.Parser t0)
fail msg = (Parsing.Parser (\input -> Parsing.ParseResultFailure (Parsing.ParseError {
  Parsing.parseErrorMessage = msg,
  Parsing.parseErrorRemainder = input})))

many :: (Parsing.Parser t0 -> Parsing.Parser [t0])
many p = (alt (some p) (pure []))

map :: ((t0 -> t1) -> Parsing.Parser t0 -> Parsing.Parser t1)
map f pa =  
  let parse = (\input -> (\x -> case x of
          Parsing.ParseResultSuccess v1 -> (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
            Parsing.parseSuccessValue = (f (Parsing.parseSuccessValue v1)),
            Parsing.parseSuccessRemainder = (Parsing.parseSuccessRemainder v1)}))
          Parsing.ParseResultFailure v1 -> (Parsing.ParseResultFailure v1)) (Parsing.unParser pa input))
  in (Parsing.Parser parse)

optional :: (Parsing.Parser t0 -> Parsing.Parser (Maybe t0))
optional p = (alt (map Maybes.pure p) (pure Nothing))

pure :: (t0 -> Parsing.Parser t0)
pure a = (Parsing.Parser (\input -> Parsing.ParseResultSuccess (Parsing.ParseSuccess {
  Parsing.parseSuccessValue = a,
  Parsing.parseSuccessRemainder = input})))

runParser :: (Parsing.Parser t0 -> String -> Parsing.ParseResult t0)
runParser p input = (Parsing.unParser p input)

-- | Parse a character (codepoint) that satisfies the given predicate
satisfy :: ((Int -> Bool) -> Parsing.Parser Int)
satisfy pred =  
  let parse = (\input ->  
          let codes = (Strings.toList input)
          in  
            let c = (Lists.head codes)
            in  
              let rest = (Strings.fromList (Lists.tail codes))
              in (Logic.ifElse (Strings.null input) (Parsing.ParseResultFailure (Parsing.ParseError {
                Parsing.parseErrorMessage = "unexpected end of input",
                Parsing.parseErrorRemainder = input})) (Logic.ifElse (pred c) (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
                Parsing.parseSuccessValue = c,
                Parsing.parseSuccessRemainder = rest})) (Parsing.ParseResultFailure (Parsing.ParseError {
                Parsing.parseErrorMessage = "character did not satisfy predicate",
                Parsing.parseErrorRemainder = input})))))
  in (Parsing.Parser parse)

sepBy :: (Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser [t0])
sepBy p sep = (alt (sepBy1 p sep) (pure []))

sepBy1 :: (Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser [t0])
sepBy1 p sep = (bind p (\x -> bind (many (bind sep (\_ -> p))) (\xs -> pure (Lists.cons x xs))))

some :: (Parsing.Parser t0 -> Parsing.Parser [t0])
some p = (bind p (\x -> bind (many p) (\xs -> pure (Lists.cons x xs))))

-- | Parse a specific string
string :: (String -> Parsing.Parser String)
string str = (Parsing.Parser (\input ->  
  let strCodes = (Strings.toList str)
  in  
    let inputCodes = (Strings.toList input)
    in  
      let strLen = (Lists.length strCodes)
      in  
        let inputPrefix = (Lists.take strLen inputCodes)
        in (Logic.ifElse (Equality.equal strCodes inputPrefix) (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
          Parsing.parseSuccessValue = str,
          Parsing.parseSuccessRemainder = (Strings.fromList (Lists.drop strLen inputCodes))})) (Parsing.ParseResultFailure (Parsing.ParseError {
          Parsing.parseErrorMessage = (Strings.cat2 "expected: " str),
          Parsing.parseErrorRemainder = input})))))
