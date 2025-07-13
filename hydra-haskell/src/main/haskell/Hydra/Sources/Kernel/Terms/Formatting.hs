module Hydra.Sources.Kernel.Terms.Formatting where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
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

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

capitalizeDef :: TElement (String -> String)
capitalizeDef = define "capitalize" $
  doc "Capitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toUpper

convertCaseDef :: TElement (CaseConvention -> CaseConvention -> String -> String)
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

convertCaseCamelToLowerSnakeDef :: TElement (String -> String)
convertCaseCamelToLowerSnakeDef = define "convertCaseCamelToLowerSnake" $
  doc "Convert a string from camel case to lower snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionLowerSnake

convertCaseCamelToUpperSnakeDef :: TElement (String -> String)
convertCaseCamelToUpperSnakeDef = define "convertCaseCamelToUpperSnake" $
  doc "Convert a string from camel case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionUpperSnake

convertCasePascalToUpperSnakeDef :: TElement (String -> String)
convertCasePascalToUpperSnakeDef = define "convertCasePascalToUpperSnake" $
  doc "Convert a string from pascal case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionPascal @@ Mantle.caseConventionUpperSnake

decapitalizeDef :: TElement (String -> String)
decapitalizeDef = define "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toLower

escapeWithUnderscoreDef :: TElement (S.Set String -> String -> String)
escapeWithUnderscoreDef = define "escapeWithUnderscore" $
  lambdas ["reserved", "s"] $
    Logic.ifElse (Sets.member (var "s") (var "reserved"))
      (var "s" ++ string "_")
      (var "s")

indentLinesDef :: TElement (String -> String)
indentLinesDef = define "indentLines" $
  lambda "s" $ lets [
    "indent">: lambda "l" $ string "    " ++ var "l"]
    $ Strings.unlines $ Lists.map (var "indent") $ Strings.lines $ var "s"

javaStyleCommentDef :: TElement (String -> String)
javaStyleCommentDef = define "javaStyleComment" $
  lambda "s" $ string "/**\n" ++ string " * " ++ var "s" ++ string "\n */"

-- TODO: simplify this helper
mapFirstLetterDef :: TElement ((String -> String) -> String -> String)
mapFirstLetterDef = define "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  lambda "mapping" $ lambda "s" $ lets [
    "firstLetter">: var "mapping" @@ (Strings.fromList (Lists.pure (Lists.head $ var "list"))),
    "list">: Strings.toList $ var "s"]
    $ Logic.ifElse
        (Strings.null $ var "s")
        (var "s")
        (Strings.cat2 (var "firstLetter") (Strings.fromList (Lists.tail $ var "list")))







nonAlnumToUnderscoresDef :: TElement (String -> String)
nonAlnumToUnderscoresDef = define "nonAlnumToUnderscores" $
  lambda "input" $ lets [
    "isAlnum">: lambda "c" $ Logic.or
      (Logic.and (Equality.gte (var "c") (char 'A')) (Equality.lte (var "c") (char 'Z')))
      (Logic.or
        (Logic.and (Equality.gte (var "c") (char 'a')) (Equality.lte (var "c") (char 'z')))
        (Logic.and (Equality.gte (var "c") (char '0')) (Equality.lte (var "c") (char '9')))),
    "replace">: lambdas ["p", "c"] $ lets [
      "s">: first $ var "p",
      "b">: second $ var "p"]
      $ Logic.ifElse (var "isAlnum" @@ var "c")
        (pair (Lists.cons (var "c") (var "s")) (boolean False))
        (Logic.ifElse (var "b")
          (pair (var "s") (boolean True))
          (pair (Lists.cons (char '_') (var "s")) (boolean True))),
    "result">: Lists.foldl (var "replace") (pair (list []) (boolean False)) (Strings.toList $ var "input")]
    $ Strings.fromList $ Lists.reverse $ first $ var "result"

sanitizeWithUnderscoresDef :: TElement (S.Set String -> String -> String)
sanitizeWithUnderscoresDef = define "sanitizeWithUnderscores" $
  lambdas ["reserved", "s"] $
    ref escapeWithUnderscoreDef @@ var "reserved" @@ (ref nonAlnumToUnderscoresDef @@ var "s")

showListDef :: TElement ((a -> String) -> [a] -> String)
showListDef = define "showList" $
  lambdas ["f", "els"] $ Strings.cat $ list [
    string "[",
    Strings.intercalate (string ", ") $ Lists.map (var "f") $ var "els",
    string "]"]

stripLeadingAndTrailingWhitespaceDef :: TElement (String -> String)
stripLeadingAndTrailingWhitespaceDef = define "stripLeadingAndTrailingWhitespace" $
  lambda "s" $
    Strings.fromList $ Lists.dropWhile (unaryFunction Chars.isSpace) $ Lists.reverse $
    Lists.dropWhile (unaryFunction Chars.isSpace) $ Lists.reverse $ Strings.toList $ var "s"

withCharacterAliasesDef :: TElement (String -> String)
withCharacterAliasesDef = define "withCharacterAliases" $
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
    "alias">: lambda "c" $ Optionals.fromMaybe
      (Lists.pure $ var "c")
      (Optionals.map (unaryFunction Strings.toList) $ Maps.lookup (var "c") (var "aliases"))]
    $ Strings.fromList $ Lists.filter (unaryFunction Chars.isAlphaNum) $ Lists.concat $
      Lists.map (var "alias") $ Strings.toList $ var "original"

wrapLineDef :: TElement (Int -> String -> String)
wrapLineDef = define "wrapLine" $
  doc "A simple soft line wrap which is suitable for code comments" $
  lambdas ["maxlen", "input"] $ lets [
    "helper">: lambdas ["prev", "rem"] $ lets [
      "trunc">: Lists.take (var "maxlen") (var "rem"),
      "spanResult">: Lists.span (lambda "c" $ Logic.and (Logic.not $ Equality.equal (var "c") (char ' ')) (Logic.not $ Equality.equal (var "c") (char '\t'))) (Lists.reverse $ var "trunc"),
      "prefix">: Lists.reverse $ second $ var "spanResult",
      "suffix">: Lists.reverse $ first $ var "spanResult"]
      $ Logic.ifElse (Equality.lte (Lists.length $ var "rem") (var "maxlen"))
        (Lists.reverse $ Lists.cons (var "rem") (var "prev"))
        (Logic.ifElse (Lists.null $ var "prefix")
          (var "helper" @@ (Lists.cons (var "trunc") (var "prev")) @@ (Lists.drop (var "maxlen") (var "rem")))
          (var "helper" @@ (Lists.cons (Lists.init $ var "prefix") (var "prev")) @@ (Lists.concat2 (var "suffix") ((Lists.drop (var "maxlen") (var "rem"))))))]
    $ Strings.fromList $ Lists.intercalate (list [char '\n']) $ var "helper" @@ (list []) @@ (Strings.toList $ var "input")
