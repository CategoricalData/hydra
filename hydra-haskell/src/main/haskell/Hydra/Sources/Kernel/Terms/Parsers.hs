{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Parsers where

-- Standard imports for kernel terms modules
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.parsers") elements
    []
    kernelTypesModules $
    Just "General-purpose parser combinators"
  where
   elements = [
     el pureDef,
     el failDef,
     el bindDef,
     el mapDef,
     el applyDef,
     el altDef,
     el eofDef,
     el satisfyDef,
     el charDef,
     el stringDef,
     el anyCharDef,
     el manyDef,
     el someDef,
     el optionalDef,
     el betweenDef,
     el sepByDef,
     el sepBy1Def,
     el choiceDef,
     el runParserDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- | A parser that always succeeds with the given value
pureDef :: TBinding (a -> Parser a)
pureDef = define "pure" $
  doc "A parser that always succeeds with the given value without consuming input" $
  "a" ~>
    Parsing.parser ("input" ~>
      Parsing.parseResultSuccess (Parsing.parseSuccess (var "a") (var "input")))

-- | A parser that always fails with the given message
failDef :: TBinding (String -> Parser a)
failDef = define "fail" $
  doc "A parser that always fails with the given error message" $
  "msg" ~>
    Parsing.parser ("input" ~>
      Parsing.parseResultFailure (Parsing.parseError (var "msg") (var "input")))

-- | Monadic bind for parsers
bindDef :: TBinding (Parser a -> (a -> Parser b) -> Parser b)
bindDef = define "bind" $
  doc "Sequence two parsers, passing the result of the first to a function that produces the second" $
  "pa" ~> "f" ~>
    Parsing.parser ("input" ~>
      cases _ParseResult (Parsing.runParser (var "pa") (var "input")) Nothing [
        _ParseResult_success>>: "s" ~>
          Parsing.runParser
            (var "f" @@ (Parsing.parseSuccessValue $ var "s"))
            (Parsing.parseSuccessRemainder $ var "s"),
        _ParseResult_failure>>: "e" ~>
          Parsing.parseResultFailure (var "e")])

-- | Map a function over the result of a parser
mapDef :: TBinding ((a -> b) -> Parser a -> Parser b)
mapDef = define "map" $
  doc "Apply a function to the result of a parser" $
  "f" ~> "pa" ~>
    Parsing.parser ("input" ~>
      cases _ParseResult (Parsing.runParser (var "pa") (var "input")) Nothing [
        _ParseResult_success>>: "s" ~>
          Parsing.parseResultSuccess (Parsing.parseSuccess
            (var "f" @@ (Parsing.parseSuccessValue $ var "s"))
            (Parsing.parseSuccessRemainder $ var "s")),
        _ParseResult_failure>>: "e" ~>
          Parsing.parseResultFailure (var "e")])

-- | Applicative apply for parsers
applyDef :: TBinding (Parser (a -> b) -> Parser a -> Parser b)
applyDef = define "apply" $
  doc "Apply a parser containing a function to a parser containing a value" $
  "pf" ~> "pa" ~>
    Parsing.parser ("input" ~>
      cases _ParseResult (Parsing.runParser (var "pf") (var "input")) Nothing [
        _ParseResult_success>>: "sf" ~>
          cases _ParseResult (Parsing.runParser (var "pa") (Parsing.parseSuccessRemainder $ var "sf")) Nothing [
            _ParseResult_success>>: "sa" ~>
              Parsing.parseResultSuccess (Parsing.parseSuccess
                ((Parsing.parseSuccessValue $ var "sf") @@ (Parsing.parseSuccessValue $ var "sa"))
                (Parsing.parseSuccessRemainder $ var "sa")),
            _ParseResult_failure>>: "e" ~>
              Parsing.parseResultFailure (var "e")],
        _ParseResult_failure>>: "e" ~>
          Parsing.parseResultFailure (var "e")])

-- | Try the first parser, if it fails try the second
altDef :: TBinding (Parser a -> Parser a -> Parser a)
altDef = define "alt" $
  doc "Try the first parser; if it fails without consuming input, try the second" $
  "p1" ~> "p2" ~>
    Parsing.parser ("input" ~>
      cases _ParseResult (Parsing.runParser (var "p1") (var "input")) Nothing [
        _ParseResult_success>>: "s" ~>
          Parsing.parseResultSuccess (var "s"),
        _ParseResult_failure>>: "e" ~>
          -- Only try alternative if no input was consumed
          Logic.ifElse (Equality.equal (Parsing.parseErrorRemainder $ var "e") (var "input"))
            (Parsing.runParser (var "p2") (var "input"))
            (Parsing.parseResultFailure (var "e"))])

-- | Parse end of input
eofDef :: TBinding (Parser ())
eofDef = define "eof" $
  doc "A parser that succeeds only at the end of input" $
  Parsing.parser ("input" ~>
    Logic.ifElse (Equality.equal (var "input") (string ""))
      (Parsing.parseResultSuccess (Parsing.parseSuccess unit (string "")))
      (Parsing.parseResultFailure (Parsing.parseError (string "expected end of input") (var "input"))))

-- | Parse a character that satisfies a predicate (characters represented as codepoints)
satisfyDef :: TBinding ((Int -> Bool) -> Parser Int)
satisfyDef = define "satisfy" $
  doc "Parse a character (codepoint) that satisfies the given predicate" $
  "pred" ~>
    Parsing.parser ("input" ~>
      Logic.ifElse (Strings.null (var "input"))
        (Parsing.parseResultFailure (Parsing.parseError (string "unexpected end of input") (var "input")))
        ("codes" <~ Strings.toList (var "input") $
         "c" <~ Lists.head (var "codes") $
         "rest" <~ Strings.fromList (Lists.tail (var "codes")) $
         Logic.ifElse (var "pred" @@ var "c")
           (Parsing.parseResultSuccess (Parsing.parseSuccess (var "c") (var "rest")))
           (Parsing.parseResultFailure (Parsing.parseError (string "character did not satisfy predicate") (var "input")))))

-- | Parse a specific character (as codepoint)
charDef :: TBinding (Int -> Parser Int)
charDef = define "char" $
  doc "Parse a specific character (codepoint)" $
  "c" ~>
    ref satisfyDef @@ ("x" ~> Equality.equal (var "x") (var "c"))

-- | Parse a specific string
stringDef :: TBinding (String -> Parser String)
stringDef = define "string" $
  doc "Parse a specific string" $
  "str" ~>
    Parsing.parser ("input" ~>
      "strCodes" <~ Strings.toList (var "str") $
      "inputCodes" <~ Strings.toList (var "input") $
      "strLen" <~ Lists.length (var "strCodes") $
      "inputPrefix" <~ Lists.take (var "strLen") (var "inputCodes") $
      Logic.ifElse (Equality.equal (var "strCodes") (var "inputPrefix"))
        (Parsing.parseResultSuccess (Parsing.parseSuccess
          (var "str")
          (Strings.fromList (Lists.drop (var "strLen") (var "inputCodes")))))
        (Parsing.parseResultFailure (Parsing.parseError
          (string "expected: " ++ var "str")
          (var "input"))))

-- | Parse any single character (as codepoint)
anyCharDef :: TBinding (Parser Int)
anyCharDef = define "anyChar" $
  doc "Parse any single character (codepoint)" $
  ref satisfyDef @@ (constant true)

-- | Parse zero or more occurrences
manyDef :: TBinding (Parser a -> Parser [a])
manyDef = define "many" $
  doc "Parse zero or more occurrences of the given parser" $
  "p" ~>
    ref altDef @@ (ref someDef @@ var "p") @@ (ref pureDef @@ list [])

-- | Parse one or more occurrences
someDef :: TBinding (Parser a -> Parser [a])
someDef = define "some" $
  doc "Parse one or more occurrences of the given parser" $
  "p" ~>
    ref bindDef @@ var "p" @@ ("x" ~>
      ref bindDef @@ (ref manyDef @@ var "p") @@ ("xs" ~>
        ref pureDef @@ (Lists.cons (var "x") (var "xs"))))

-- | Optionally parse something
optionalDef :: TBinding (Parser a -> Parser (Maybe a))
optionalDef = define "optional" $
  doc "Optionally parse something, returning Nothing if it fails" $
  "p" ~>
    ref altDef
      @@ (ref mapDef @@ (unaryFunction just) @@ var "p")
      @@ (ref pureDef @@ nothing)

-- | Parse something between two other parsers
betweenDef :: TBinding (Parser open -> Parser close -> Parser a -> Parser a)
betweenDef = define "between" $
  doc "Parse something between an opening and closing parser" $
  "open" ~> "close" ~> "p" ~>
    ref bindDef @@ var "open" @@ (constant $
      ref bindDef @@ var "p" @@ ("x" ~>
        ref bindDef @@ var "close" @@ (constant $
          ref pureDef @@ var "x")))

-- | Parse zero or more occurrences separated by a separator
sepByDef :: TBinding (Parser a -> Parser sep -> Parser [a])
sepByDef = define "sepBy" $
  doc "Parse zero or more occurrences separated by a separator" $
  "p" ~> "sep" ~>
    ref altDef @@ (ref sepBy1Def @@ var "p" @@ var "sep") @@ (ref pureDef @@ list [])

-- | Parse one or more occurrences separated by a separator
sepBy1Def :: TBinding (Parser a -> Parser sep -> Parser [a])
sepBy1Def = define "sepBy1" $
  doc "Parse one or more occurrences separated by a separator" $
  "p" ~> "sep" ~>
    ref bindDef @@ var "p" @@ ("x" ~>
      ref bindDef @@ (ref manyDef @@ (ref bindDef @@ var "sep" @@ (constant $ var "p"))) @@ ("xs" ~>
        ref pureDef @@ (Lists.cons (var "x") (var "xs"))))

-- | Try parsers in order until one succeeds
choiceDef :: TBinding ([Parser a] -> Parser a)
choiceDef = define "choice" $
  doc "Try each parser in the list until one succeeds" $
  "ps" ~>
    Lists.foldl (ref altDef) (ref failDef @@ string "no choice matched") (var "ps")

-- | Run a parser on input and return the result
runParserDef :: TBinding (Parser a -> String -> ParseResult a)
runParserDef = define "runParser" $
  doc "Run a parser on the given input string" $
  "p" ~> "input" ~>
    Parsing.runParser (var "p") (var "input")
