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

-- | Try the first parser; if it fails without consuming input, try the second
alt :: Parsing.Parser t0 -> Parsing.Parser t0 -> Parsing.Parser t0
alt p1 p2 =

      let parse =
              \input -> (\x -> case x of
                Parsing.ParseResultSuccess v0 -> Parsing.ParseResultSuccess v0
                Parsing.ParseResultFailure v0 -> Logic.ifElse (Equality.equal (Parsing.parseErrorRemainder v0) input) (Parsing.unParser p2 input) (Parsing.ParseResultFailure v0)) (Parsing.unParser p1 input)
      in (Parsing.Parser parse)

-- | Parse any single character (codepoint)
anyChar :: Parsing.Parser Int
anyChar = satisfy (\_ -> True)

-- | Apply a parser containing a function to a parser containing a value
apply :: Parsing.Parser (t0 -> t1) -> Parsing.Parser t0 -> Parsing.Parser t1
apply pf pa =

      let parse =
              \input -> (\x -> case x of
                Parsing.ParseResultSuccess v0 -> (\x -> case x of
                  Parsing.ParseResultSuccess v1 -> Parsing.ParseResultSuccess (Parsing.ParseSuccess {
                    Parsing.parseSuccessValue = (Parsing.parseSuccessValue v0 (Parsing.parseSuccessValue v1)),
                    Parsing.parseSuccessRemainder = (Parsing.parseSuccessRemainder v1)})
                  Parsing.ParseResultFailure v1 -> Parsing.ParseResultFailure v1) (Parsing.unParser pa (Parsing.parseSuccessRemainder v0))
                Parsing.ParseResultFailure v0 -> Parsing.ParseResultFailure v0) (Parsing.unParser pf input)
      in (Parsing.Parser parse)

-- | Parse something between an opening and closing parser
between :: Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser t2 -> Parsing.Parser t2
between open close p = bind open (\_ -> bind p (\x -> bind close (\_2 -> pure x)))

-- | Sequence two parsers, passing the result of the first to a function that produces the second
bind :: Parsing.Parser t0 -> (t0 -> Parsing.Parser t1) -> Parsing.Parser t1
bind pa f =

      let parse =
              \input -> (\x -> case x of
                Parsing.ParseResultSuccess v0 -> Parsing.unParser (f (Parsing.parseSuccessValue v0)) (Parsing.parseSuccessRemainder v0)
                Parsing.ParseResultFailure v0 -> Parsing.ParseResultFailure v0) (Parsing.unParser pa input)
      in (Parsing.Parser parse)

-- | Parse a specific character (codepoint)
char :: Int -> Parsing.Parser Int
char c = satisfy (\x -> Equality.equal x c)

-- | Try each parser in the list until one succeeds
choice :: [Parsing.Parser t0] -> Parsing.Parser t0
choice ps = Lists.foldl alt (fail "no choice matched") ps

-- | A parser that succeeds only at the end of input
eof :: Parsing.Parser ()
eof =
    Parsing.Parser (\input -> Logic.ifElse (Equality.equal input "") (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
      Parsing.parseSuccessValue = (),
      Parsing.parseSuccessRemainder = ""})) (Parsing.ParseResultFailure (Parsing.ParseError {
      Parsing.parseErrorMessage = "expected end of input",
      Parsing.parseErrorRemainder = input})))

-- | A parser that always fails with the given error message
fail :: String -> Parsing.Parser t0
fail msg =
    Parsing.Parser (\input -> Parsing.ParseResultFailure (Parsing.ParseError {
      Parsing.parseErrorMessage = msg,
      Parsing.parseErrorRemainder = input}))

-- | Create a parser that defers construction of another parser until parsing time. This is essential for breaking recursive parser definitions.
lazy :: (() -> Parsing.Parser t0) -> Parsing.Parser t0
lazy f = Parsing.Parser (\input -> Parsing.unParser (f ()) input)

-- | Parse zero or more occurrences of the given parser
many :: Parsing.Parser t0 -> Parsing.Parser [t0]
many p = alt (some p) (pure [])

-- | Apply a function to the result of a parser
map :: (t0 -> t1) -> Parsing.Parser t0 -> Parsing.Parser t1
map f pa =

      let parse =
              \input -> (\x -> case x of
                Parsing.ParseResultSuccess v0 -> Parsing.ParseResultSuccess (Parsing.ParseSuccess {
                  Parsing.parseSuccessValue = (f (Parsing.parseSuccessValue v0)),
                  Parsing.parseSuccessRemainder = (Parsing.parseSuccessRemainder v0)})
                Parsing.ParseResultFailure v0 -> Parsing.ParseResultFailure v0) (Parsing.unParser pa input)
      in (Parsing.Parser parse)

-- | Optionally parse something, returning Nothing if it fails
optional :: Parsing.Parser t0 -> Parsing.Parser (Maybe t0)
optional p = alt (map Maybes.pure p) (pure Nothing)

-- | A parser that always succeeds with the given value without consuming input
pure :: t0 -> Parsing.Parser t0
pure a =
    Parsing.Parser (\input -> Parsing.ParseResultSuccess (Parsing.ParseSuccess {
      Parsing.parseSuccessValue = a,
      Parsing.parseSuccessRemainder = input}))

-- | Run a parser on the given input string
runParser :: Parsing.Parser t0 -> String -> Parsing.ParseResult t0
runParser p input = Parsing.unParser p input

-- | Parse a character (codepoint) that satisfies the given predicate
satisfy :: (Int -> Bool) -> Parsing.Parser Int
satisfy pred =

      let parse =
              \input ->
                let codes = Strings.toList input
                in (Maybes.maybe (Parsing.ParseResultFailure (Parsing.ParseError {
                  Parsing.parseErrorMessage = "unexpected end of input",
                  Parsing.parseErrorRemainder = input})) (\c ->
                  let rest = Strings.fromList (Lists.drop 1 codes)
                  in (Logic.ifElse (pred c) (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
                    Parsing.parseSuccessValue = c,
                    Parsing.parseSuccessRemainder = rest})) (Parsing.ParseResultFailure (Parsing.ParseError {
                    Parsing.parseErrorMessage = "character did not satisfy predicate",
                    Parsing.parseErrorRemainder = input})))) (Lists.maybeHead codes))
      in (Parsing.Parser parse)

-- | Parse zero or more occurrences separated by a separator
sepBy :: Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser [t0]
sepBy p sep = alt (sepBy1 p sep) (pure [])

-- | Parse one or more occurrences separated by a separator
sepBy1 :: Parsing.Parser t0 -> Parsing.Parser t1 -> Parsing.Parser [t0]
sepBy1 p sep = bind p (\x -> bind (many (bind sep (\_ -> p))) (\xs -> pure (Lists.cons x xs)))

-- | Parse one or more occurrences of the given parser
some :: Parsing.Parser t0 -> Parsing.Parser [t0]
some p = bind p (\x -> bind (many p) (\xs -> pure (Lists.cons x xs)))

-- | Parse a specific string
string :: String -> Parsing.Parser String
string str =
    Parsing.Parser (\input ->
      let strCodes = Strings.toList str
          inputCodes = Strings.toList input
          strLen = Lists.length strCodes
          inputPrefix = Lists.take strLen inputCodes
      in (Logic.ifElse (Equality.equal strCodes inputPrefix) (Parsing.ParseResultSuccess (Parsing.ParseSuccess {
        Parsing.parseSuccessValue = str,
        Parsing.parseSuccessRemainder = (Strings.fromList (Lists.drop strLen inputCodes))})) (Parsing.ParseResultFailure (Parsing.ParseError {
        Parsing.parseErrorMessage = (Strings.cat2 "expected: " str),
        Parsing.parseErrorRemainder = input}))))
