module Hydra.Sources.Kernel.Terms.Formatting where

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
module_ = Module (Namespace "hydra.formatting") elements
    []
    kernelTypesModules $
    Just "String formatting types and functions."
  where
    elements = [
      el capitalizeDef,
      el convertCaseDef,
      el convertCaseCamelToLowerSnakeDef,
      el convertCaseCamelToUpperSnakeDef,
      el convertCasePascalToUpperSnakeDef,
      el decapitalizeDef,
      el escapeWithUnderscoreDef,
      el indentLinesDef,
      el javaStyleCommentDef,
      el mapFirstLetterDef,
      el nonAlnumToUnderscoresDef,
      el sanitizeWithUnderscoresDef,
      el showListDef,
      el stripLeadingAndTrailingWhitespaceDef,
      el withCharacterAliasesDef,
      el wrapLineDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

capitalizeDef :: TBinding (String -> String)
capitalizeDef = define "capitalize" $
  doc "Capitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toUpper

convertCaseDef :: TBinding (CaseConvention -> CaseConvention -> String -> String)
convertCaseDef = define "convertCase" $
  doc "Convert a string from one case convention to another" $
  lambdas ["from", "to", "original"] $ lets [
    "parts">: lets [
      "byCaps">: lets [
        "splitOnUppercase">: lambda "acc" $ lambda "c" $ Lists.concat2
          (Logic.ifElse (Chars.isUpper $ var "c") emptyParts (list []))
          (Lists.cons (Lists.cons (var "c") (Lists.head $ var "acc")) (Lists.tail $ var "acc"))]
        $ Lists.map (primitive _strings_fromList) $ Lists.foldl (var "splitOnUppercase") emptyParts
          $ Lists.reverse $ Strings.toList (ref decapitalizeDef @@ var "original"),
      "byUnderscores">: Strings.splitOn (string "_") $ var "original"]
      $ (match _CaseConvention Nothing [
        _CaseConvention_camel>>: constant $ var "byCaps",
        _CaseConvention_pascal>>: constant $ var "byCaps",
        _CaseConvention_lowerSnake>>: constant $ var "byUnderscores",
        _CaseConvention_upperSnake>>: constant $ var "byUnderscores"]) @@ var "from"]
    $ (match _CaseConvention Nothing [
      _CaseConvention_camel>>: constant $ ref decapitalizeDef @@ (Strings.cat (Lists.map (ref capitalizeDef <.> primitive _strings_toLower) $ var "parts")),
      _CaseConvention_pascal>>: constant $ Strings.cat (Lists.map (ref capitalizeDef <.> primitive _strings_toLower) $ var "parts"),
      _CaseConvention_lowerSnake>>: constant $ Strings.intercalate (string "_") (Lists.map (primitive _strings_toLower) $ var "parts"),
      _CaseConvention_upperSnake>>: constant $ Strings.intercalate (string "_") (Lists.map (primitive _strings_toUpper) $ var "parts")
      ]) @@ var "to"
  where
    emptyParts = list [list []]

convertCaseCamelToLowerSnakeDef :: TBinding (String -> String)
convertCaseCamelToLowerSnakeDef = define "convertCaseCamelToLowerSnake" $
  doc "Convert a string from camel case to lower snake case" $
  ref convertCaseDef @@ Util.caseConventionCamel @@ Util.caseConventionLowerSnake

convertCaseCamelToUpperSnakeDef :: TBinding (String -> String)
convertCaseCamelToUpperSnakeDef = define "convertCaseCamelToUpperSnake" $
  doc "Convert a string from camel case to upper snake case" $
  ref convertCaseDef @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake

convertCasePascalToUpperSnakeDef :: TBinding (String -> String)
convertCasePascalToUpperSnakeDef = define "convertCasePascalToUpperSnake" $
  doc "Convert a string from pascal case to upper snake case" $
  ref convertCaseDef @@ Util.caseConventionPascal @@ Util.caseConventionUpperSnake

decapitalizeDef :: TBinding (String -> String)
decapitalizeDef = define "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toLower

escapeWithUnderscoreDef :: TBinding (S.Set String -> String -> String)
escapeWithUnderscoreDef = define "escapeWithUnderscore" $
  doc "Escape reserved words by appending an underscore" $
  lambdas ["reserved", "s"] $
    Logic.ifElse (Sets.member (var "s") (var "reserved"))
      (var "s" ++ string "_")
      (var "s")

indentLinesDef :: TBinding (String -> String)
indentLinesDef = define "indentLines" $
  doc "Indent each line of a string with four spaces" $
  lambda "s" $ lets [
    "indent">: lambda "l" $ string "    " ++ var "l"]
    $ Strings.unlines $ Lists.map (var "indent") $ Strings.lines $ var "s"

javaStyleCommentDef :: TBinding (String -> String)
javaStyleCommentDef = define "javaStyleComment" $
  doc "Format a string as a Java-style block comment" $
  lambda "s" $ string "/**\n" ++ string " * " ++ var "s" ++ string "\n */"

-- TODO: simplify this helper
mapFirstLetterDef :: TBinding ((String -> String) -> String -> String)
mapFirstLetterDef = define "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  "mapping" ~> "s" ~>
  "list" <~ Strings.toList (var "s") $
  "firstLetter" <~ var "mapping" @@ (Strings.fromList (Lists.pure (Lists.head $ var "list"))) $
  Logic.ifElse
    (Strings.null $ var "s")
    (var "s")
    (Strings.cat2 (var "firstLetter") (Strings.fromList (Lists.tail $ var "list")))

nonAlnumToUnderscoresDef :: TBinding (String -> String)
nonAlnumToUnderscoresDef = define "nonAlnumToUnderscores" $
  doc "Replace sequences of non-alphanumeric characters with single underscores" $
  "input" ~>
  "isAlnum" <~ ("c" ~> Logic.or
    (Logic.and (Equality.gte (var "c") (char 'A')) (Equality.lte (var "c") (char 'Z')))
    (Logic.or
      (Logic.and (Equality.gte (var "c") (char 'a')) (Equality.lte (var "c") (char 'z')))
      (Logic.and (Equality.gte (var "c") (char '0')) (Equality.lte (var "c") (char '9'))))) $
  "replace" <~ ("p" ~> "c" ~>
    "s" <~ Pairs.first (var "p") $
    "b" <~ Pairs.second (var "p") $
    Logic.ifElse (var "isAlnum" @@ var "c")
      (pair (Lists.cons (var "c") (var "s")) (boolean False))
      (Logic.ifElse (var "b")
        (pair (var "s") (boolean True))
        (pair (Lists.cons (char '_') (var "s")) (boolean True)))) $
  "result" <~ Lists.foldl (var "replace") (pair (list []) (boolean False)) (Strings.toList $ var "input") $
  Strings.fromList $ Lists.reverse $ Pairs.first $ var "result"

sanitizeWithUnderscoresDef :: TBinding (S.Set String -> String -> String)
sanitizeWithUnderscoresDef = define "sanitizeWithUnderscores" $
  doc "Sanitize a string by replacing non-alphanumeric characters and escaping reserved words" $
  "reserved" ~> "s" ~> ref escapeWithUnderscoreDef @@ var "reserved" @@ (ref nonAlnumToUnderscoresDef @@ var "s")

showListDef :: TBinding ((a -> String) -> [a] -> String)
showListDef = define "showList" $
  doc "Format a list of elements as a bracketed, comma-separated string" $
  "f" ~> "els" ~> Strings.cat $ list [
    string "[",
    Strings.intercalate (string ", ") $ Lists.map (var "f") $ var "els",
    string "]"]

stripLeadingAndTrailingWhitespaceDef :: TBinding (String -> String)
stripLeadingAndTrailingWhitespaceDef = define "stripLeadingAndTrailingWhitespace" $
  doc "Remove leading and trailing whitespace from a string" $
  "s" ~> Strings.fromList $ Lists.dropWhile (unaryFunction Chars.isSpace) $ Lists.reverse $
    Lists.dropWhile (unaryFunction Chars.isSpace) $ Lists.reverse $ Strings.toList $ var "s"

withCharacterAliasesDef :: TBinding (String -> String)
withCharacterAliasesDef = define "withCharacterAliases" $
  doc "Replace special characters with their alphanumeric aliases" $
  lambda "original" $ lets [
    -- Taken from: https://cs.stanford.edu/people/miles/iso8859.html
    "aliases">: Maps.fromList $ list [
      pair (int32 32) (string "sp"),
      pair (int32 33) (string "excl"),
      pair (int32 34) (string "quot"),
      pair (int32 35) (string "num"),
      pair (int32 36) (string "dollar"),
      pair (int32 37) (string "percnt"),
      pair (int32 38) (string "amp"),
      pair (int32 39) (string "apos"),
      pair (int32 40) (string "lpar"),
      pair (int32 41) (string "rpar"),
      pair (int32 42) (string "ast"),
      pair (int32 43) (string "plus"),
      pair (int32 44) (string "comma"),
      pair (int32 45) (string "minus"),
      pair (int32 46) (string "period"),
      pair (int32 47) (string "sol"),
      pair (int32 58) (string "colon"),
      pair (int32 59) (string "semi"),
      pair (int32 60) (string "lt"),
      pair (int32 61) (string "equals"),
      pair (int32 62) (string "gt"),
      pair (int32 63) (string "quest"),
      pair (int32 64) (string "commat"),
      pair (int32 91) (string "lsqb"),
      pair (int32 92) (string "bsol"),
      pair (int32 93) (string "rsqb"),
      pair (int32 94) (string "circ"),
      pair (int32 95) (string "lowbar"),
      pair (int32 96) (string "grave"),
      pair (int32 123) (string "lcub"),
      pair (int32 124) (string "verbar"),
      pair (int32 125) (string "rcub"),
      pair (int32 126) (string "tilde")],
    "alias">: lambda "c" $ Maybes.fromMaybe
      (Lists.pure $ var "c")
      (Maybes.map (unaryFunction Strings.toList) $ Maps.lookup (var "c") (var "aliases"))]
    $ Strings.fromList $ Lists.filter (unaryFunction Chars.isAlphaNum) $ Lists.concat $
      Lists.map (var "alias") $ Strings.toList $ var "original"

wrapLineDef :: TBinding (Int -> String -> String)
wrapLineDef = define "wrapLine" $
  doc "A simple soft line wrap which is suitable for code comments" $
  lambdas ["maxlen", "input"] $ lets [
    "helper">: lambdas ["prev", "rem"] $ lets [
      "trunc">: Lists.take (var "maxlen") (var "rem"),
      "spanResult">: Lists.span
        (lambda "c" $ Logic.and (Logic.not $ Equality.equal (var "c") (char ' ')) (Logic.not $ Equality.equal (var "c") (char '\t')))
        (Lists.reverse $ var "trunc"),
      "prefix">: Lists.reverse $ Pairs.second $ var "spanResult",
      "suffix">: Lists.reverse $ Pairs.first $ var "spanResult"]
      $ Logic.ifElse (Equality.lte (Lists.length $ var "rem") (var "maxlen"))
        (Lists.reverse $ Lists.cons (var "rem") (var "prev"))
        (Logic.ifElse (Lists.null $ var "prefix")
          (var "helper" @@ (Lists.cons (var "trunc") (var "prev")) @@ (Lists.drop (var "maxlen") (var "rem")))
          (var "helper" @@ (Lists.cons (Lists.init $ var "prefix") (var "prev")) @@ (Lists.concat2 (var "suffix") ((Lists.drop (var "maxlen") (var "rem"))))))]
    $ Strings.fromList $ Lists.intercalate (list [char '\n']) $ var "helper" @@ (list []) @@ (Strings.toList $ var "input")
