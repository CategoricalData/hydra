
module Hydra.Sources.Kernel.Terms.Parsers where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (map)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (apply, bind, char, map)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), pure, fail, map)
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: Namespace
ns = Namespace "hydra.parsers"

module_ :: Module
module_ = Module ns definitions
    []
    kernelTypesNamespaces $
    Just "General-purpose parser combinators"
  where
   definitions = [
     toDefinition alt,
     toDefinition anyChar,
     toDefinition apply,
     toDefinition between,
     toDefinition bind,
     toDefinition char,
     toDefinition choice,
     toDefinition eof,
     toDefinition fail,
     toDefinition lazy,
     toDefinition many,
     toDefinition map,
     toDefinition optional,
     toDefinition pure,
     toDefinition runParser,
     toDefinition satisfy,
     toDefinition sepBy,
     toDefinition sepBy1,
     toDefinition some,
     toDefinition string_]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Try the first parser, if it fails try the second
alt :: TTermDefinition (Parser a -> Parser a -> Parser a)
alt = define "alt" $
  doc "Try the first parser; if it fails without consuming input, try the second" $
  "p1" ~> "p2" ~>
  "parse" <~ ("input" ~> cases _ParseResult (unwrap _Parser @@ (var "p1") @@ (var "input")) Nothing [
    _ParseResult_success>>: "s" ~>
      Parsing.parseResultSuccess (var "s"),
    _ParseResult_failure>>: "e" ~>
      -- Only try alternative if no input was consumed
      Logic.ifElse (Equality.equal (Parsing.parseErrorRemainder $ var "e") (var "input"))
        (unwrap _Parser @@ (var "p2") @@ (var "input"))
        (Parsing.parseResultFailure (var "e"))]) $
  Parsing.parser (var "parse")

-- | Parse any single character (as codepoint)
anyChar :: TTermDefinition (Parser Int)
anyChar = define "anyChar" $
  doc "Parse any single character (codepoint)" $
  satisfy @@ (constant true)

-- | Applicative apply for parsers
apply :: TTermDefinition (Parser (a -> b) -> Parser a -> Parser b)
apply = define "apply" $
  doc "Apply a parser containing a function to a parser containing a value" $
  "pf" ~> "pa" ~>
  "parse" <~ ("input" ~> cases _ParseResult (unwrap _Parser @@ (var "pf") @@ (var "input")) Nothing [
    _ParseResult_success>>: "sf" ~>
      cases _ParseResult (unwrap _Parser @@ (var "pa") @@ (Parsing.parseSuccessRemainder $ var "sf")) Nothing [
        _ParseResult_success>>: "sa" ~>
          Parsing.parseResultSuccess (Parsing.parseSuccess
            ((Parsing.parseSuccessValue $ var "sf") @@ (Parsing.parseSuccessValue $ var "sa"))
            (Parsing.parseSuccessRemainder $ var "sa")),
        _ParseResult_failure>>: "e" ~>
          Parsing.parseResultFailure (var "e")],
    _ParseResult_failure>>: "e" ~>
      Parsing.parseResultFailure (var "e")]) $
  Parsing.parser (var "parse")

-- | Parse something between two other parsers
between :: TTermDefinition (Parser open -> Parser close -> Parser a -> Parser a)
between = define "between" $
  doc "Parse something between an opening and closing parser" $
  "open" ~> "close" ~> "p" ~>
    bind @@ var "open" @@ (constant $
      bind @@ var "p" @@ ("x" ~>
        bind @@ var "close" @@ (constant $
          pure @@ var "x")))

-- | Monadic bind for parsers
bind :: TTermDefinition (Parser a -> (a -> Parser b) -> Parser b)
bind = define "bind" $
  doc "Sequence two parsers, passing the result of the first to a function that produces the second" $
  "pa" ~> "f" ~>
  "parse" <~ ("input" ~> cases _ParseResult (unwrap _Parser @@ (var "pa") @@ (var "input")) Nothing [
    _ParseResult_success>>: "s" ~>
      unwrap _Parser @@
        (var "f" @@ (Parsing.parseSuccessValue $ var "s")) @@
        (Parsing.parseSuccessRemainder $ var "s"),
    _ParseResult_failure>>: "e" ~>
      Parsing.parseResultFailure (var "e")]) $
  Parsing.parser (var "parse")

-- | Parse a specific character (as codepoint)
char :: TTermDefinition (Int -> Parser Int)
char = define "char" $
  doc "Parse a specific character (codepoint)" $
  "c" ~>
    satisfy @@ ("x" ~> Equality.equal (var "x") (var "c"))

-- | Try parsers in order until one succeeds
choice :: TTermDefinition ([Parser a] -> Parser a)
choice = define "choice" $
  doc "Try each parser in the list until one succeeds" $
  "ps" ~>
    Lists.foldl alt (fail @@ string "no choice matched") (var "ps")

-- | Parse end of input
eof :: TTermDefinition (Parser ())
eof = define "eof" $
  doc "A parser that succeeds only at the end of input" $
  Parsing.parser ("input" ~>
    Logic.ifElse (Equality.equal (var "input") (string ""))
      (Parsing.parseResultSuccess (Parsing.parseSuccess unit (string "")))
      (Parsing.parseResultFailure (Parsing.parseError (string "expected end of input") (var "input"))))

-- | A parser that always fails with the given message
fail :: TTermDefinition (String -> Parser a)
fail = define "fail" $
  doc "A parser that always fails with the given error message" $
  "msg" ~>
    Parsing.parser ("input" ~>
      Parsing.parseResultFailure (Parsing.parseError (var "msg") (var "input")))

-- | Create a parser that defers construction of another parser until parsing time.
-- This is essential for breaking recursive parser definitions.
lazy :: TTermDefinition ((() -> Parser a) -> Parser a)
lazy = define "lazy" $
  doc ("Create a parser that defers construction of another parser until parsing time."
    <> " This is essential for breaking recursive parser definitions.") $
  "f" ~>
  Parsing.parser ("input" ~>
    unwrap _Parser @@ (var "f" @@ unit) @@ (var "input"))

-- | Parse zero or more occurrences
many :: TTermDefinition (Parser a -> Parser [a])
many = define "many" $
  doc "Parse zero or more occurrences of the given parser" $
  "p" ~>
    alt @@ (some @@ var "p") @@ (pure @@ list ([] :: [TTerm a]))

-- | Map a function over the result of a parser
map :: TTermDefinition ((a -> b) -> Parser a -> Parser b)
map = define "map" $
  doc "Apply a function to the result of a parser" $
  "f" ~> "pa" ~>
  "parse" <~ ("input" ~> cases _ParseResult (unwrap _Parser @@ (var "pa") @@ (var "input")) Nothing [
    _ParseResult_success>>: "s" ~>
      Parsing.parseResultSuccess (Parsing.parseSuccess
        (var "f" @@ (Parsing.parseSuccessValue $ var "s"))
        (Parsing.parseSuccessRemainder $ var "s")),
    _ParseResult_failure>>: "e" ~>
      Parsing.parseResultFailure (var "e")]) $
  Parsing.parser (var "parse")

-- | Optionally parse something
optional :: TTermDefinition (Parser a -> Parser (Maybe a))
optional = define "optional" $
  doc "Optionally parse something, returning Nothing if it fails" $
  "p" ~>
    alt
      @@ (map @@ (unaryFunction just) @@ var "p")
      @@ (pure @@ nothing)

-- | A parser that always succeeds with the given value
pure :: TTermDefinition (a -> Parser a)
pure = define "pure" $
  doc "A parser that always succeeds with the given value without consuming input" $
  "a" ~>
    Parsing.parser ("input" ~>
      Parsing.parseResultSuccess (Parsing.parseSuccess (var "a") (var "input")))

-- | Run a parser on input and return the result
runParser :: TTermDefinition (Parser a -> String -> ParseResult a)
runParser = define "runParser" $
  doc "Run a parser on the given input string" $
  "p" ~> "input" ~>
    unwrap _Parser @@ (var "p") @@ (var "input")

-- | Parse a character that satisfies a predicate (characters represented as codepoints)
satisfy :: TTermDefinition ((Int -> Bool) -> Parser Int)
satisfy = define "satisfy" $
  doc "Parse a character (codepoint) that satisfies the given predicate" $
  "pred" ~>
  "parse" <~ ("input" ~>
    "codes" <~ Strings.toList (var "input") $
    Maybes.maybe
      (Parsing.parseResultFailure (Parsing.parseError (string "unexpected end of input") (var "input")))
      ("c" ~>
       "rest" <~ Strings.fromList (Lists.drop (int32 1) (var "codes")) $
       Logic.ifElse (var "pred" @@ var "c")
         (Parsing.parseResultSuccess (Parsing.parseSuccess (var "c") (var "rest")))
         (Parsing.parseResultFailure (Parsing.parseError (string "character did not satisfy predicate") (var "input"))))
      (Lists.safeHead $ var "codes")) $
  Parsing.parser (var "parse")

-- | Parse zero or more occurrences separated by a separator
sepBy :: TTermDefinition (Parser a -> Parser sep -> Parser [a])
sepBy = define "sepBy" $
  doc "Parse zero or more occurrences separated by a separator" $
  "p" ~> "sep" ~>
    alt @@ (sepBy1 @@ var "p" @@ var "sep") @@ (pure @@ list ([] :: [TTerm a]))

-- | Parse one or more occurrences separated by a separator
sepBy1 :: TTermDefinition (Parser a -> Parser sep -> Parser [a])
sepBy1 = define "sepBy1" $
  doc "Parse one or more occurrences separated by a separator" $
  "p" ~> "sep" ~>
    bind @@ var "p" @@ ("x" ~>
      bind @@ (many @@ (bind @@ var "sep" @@ (constant $ var "p"))) @@ ("xs" ~>
        pure @@ (Lists.cons (var "x") (var "xs"))))

-- | Parse one or more occurrences
some :: TTermDefinition (Parser a -> Parser [a])
some = define "some" $
  doc "Parse one or more occurrences of the given parser" $
  "p" ~>
    bind @@ var "p" @@ ("x" ~>
      bind @@ (many @@ var "p") @@ ("xs" ~>
        pure @@ (Lists.cons (var "x") (var "xs"))))

-- | Parse a specific string
string_ :: TTermDefinition (String -> Parser String)
string_ = define "string" $
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
