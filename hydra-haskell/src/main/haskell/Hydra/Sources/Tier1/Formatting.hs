module Hydra.Sources.Tier1.Formatting where

-- Standard term-level Tier-1 imports
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Mantle as Mantle
import Hydra.Sources.Tier1.Mantle
import Hydra.Sources.Libraries


formattingDefinition :: String -> TTerm a -> TElement a
formattingDefinition = definitionInModule hydraFormattingModule

hydraFormattingModule :: Module
hydraFormattingModule = Module (Namespace "hydra.formatting") elements [] [hydraMantleModule] $
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
      el mapFirstLetterDef]

capitalizeDef :: TElement (String -> String)
capitalizeDef = formattingDefinition "capitalize" $
  doc "Capitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toUpper

convertCaseDef :: TElement (CaseConvention -> CaseConvention -> String -> String)
convertCaseDef = formattingDefinition "convertCase" $
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
convertCaseCamelToLowerSnakeDef = formattingDefinition "convertCaseCamelToLowerSnake" $
  doc "Convert a string from camel case to lower snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionLowerSnake

convertCaseCamelToUpperSnakeDef :: TElement (String -> String)
convertCaseCamelToUpperSnakeDef = formattingDefinition "convertCaseCamelToUpperSnake" $
  doc "Convert a string from camel case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionUpperSnake

convertCasePascalToUpperSnakeDef :: TElement (String -> String)
convertCasePascalToUpperSnakeDef = formattingDefinition "convertCasePascalToUpperSnake" $
  doc "Convert a string from pascal case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionPascal @@ Mantle.caseConventionUpperSnake

decapitalizeDef :: TElement (String -> String)
decapitalizeDef = formattingDefinition "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toLower

escapeWithUnderscoreDef :: TElement (S.Set String -> String -> String)
escapeWithUnderscoreDef = formattingDefinition "escapeWithUnderscore" $
  lambdas ["reserved", "s"] $
    Logic.ifElse (Sets.member (var "s") (var "reserved"))
      (var "s" ++ string "_")
      (var "s")

indentLinesDef :: TElement (String -> String)
indentLinesDef = formattingDefinition "indentLines" $
  lambda "s" $ lets [
    "indent">: lambda "l" $ string "    " ++ var "l"]
    $ Strings.unlines $ Lists.map (var "indent") $ Strings.lines $ var "s"

javaStyleCommentDef :: TElement (String -> String)
javaStyleCommentDef = formattingDefinition "javaStyleComment" $
  lambda "s" $ string "/**\n" ++ string " * " ++ var "s" ++ string "\n */"

-- TODO: simplify this helper
mapFirstLetterDef :: TElement ((String -> String) -> String -> String)
mapFirstLetterDef = formattingDefinition "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  lambda "mapping" $ lambda "s" $ lets [
    "firstLetter">: var "mapping" @@ (Strings.fromList (Lists.pure (Lists.head $ var "list"))),
    "list">: Strings.toList $ var "s"]
    $ Logic.ifElse
        (Strings.isEmpty $ var "s")
        (var "s")
        (Strings.cat2 (var "firstLetter") (Strings.fromList (Lists.tail $ var "list")))









--nonAlnumToUnderscoresDef :: TElement (String -> String)
--nonAlnumToUnderscoresDef = formattingDefinition "nonAlnumToUnderscores" $
--  lambda "input" $ lets [
--    "isAlnum">: lambda "c" $ Logic.or
--      (Logic.and (Chars.gteChar (var "c") (Chars.char 'A')) (Chars.lteChar (var "c") (Chars.char 'Z')))
--      (Logic.or
--        (Logic.and (Chars.gteChar (var "c") (Chars.char 'a')) (Chars.lteChar (var "c") (Chars.char 'z')))
--        (Logic.and (Chars.gteChar (var "c") (Chars.char '0')) (Chars.lteChar (var "c") (Chars.char '9')))),
--    "replace">: lambdas ["p", "c"] $ lets [
--      "s">: first $ var "p",
--      "b">: second $ var "p"]
--      $ Logic.ifElse (var "isAlnum" @@ var "c")
--        (pair (Lists.cons (var "c") (var "s")) (boolean False))
--        (Logic.ifElse (var "b")
--          (pair (var "s") (boolean True))
--          (pair (Lists.cons (Chars.char '_') (var "s")) (boolean True))),
--    "result">: Lists.foldl (var "replace") (pair (list []) (boolean False)) (Strings.toList $ var "input")]
--    $ Strings.fromList $ Lists.reverse $ first $ var "result"

--sanitizeWithUnderscoresDef :: TElement (S.Set String -> String -> String)
--sanitizeWithUnderscoresDef = formattingDefinition "sanitizeWithUnderscores" $
--  lambdas ["reserved", "s"] $
--    ref escapeWithUnderscoreDef @@ var "reserved" @@ (ref nonAlnumToUnderscoresDef @@ var "s")
--
--stripLeadingAndTrailingWhitespaceDef :: TElement (String -> String)
--stripLeadingAndTrailingWhitespaceDef = formattingDefinition "stripLeadingAndTrailingWhitespace" $
--  lambda "s" $
--    Strings.fromList $ Lists.dropWhile Chars.isSpace $ Lists.reverse $
--    Lists.dropWhile Chars.isSpace $ Lists.reverse $ Strings.toList $ var "s"

--withCharacterAliasesDef :: TElement (String -> String)
--withCharacterAliasesDef = formattingDefinition "withCharacterAliases" $
--  lambda "original" $ lets [
--    "aliases">: Maps.fromList $ list [
--      pair (int32 32) (string "sp"),
--      pair (int32 33) (string "excl"),
--      pair (int32 34) (string "quot"),
--      pair (int32 35) (string "num"),
--      pair (int32 36) (string "dollar"),
--      pair (int32 37) (string "percnt"),
--      pair (int32 38) (string "amp"),
--      pair (int32 39) (string "apos"),
--      pair (int32 40) (string "lpar"),
--      pair (int32 41) (string "rpar"),
--      pair (int32 42) (string "ast"),
--      pair (int32 43) (string "plus"),
--      pair (int32 44) (string "comma"),
--      pair (int32 45) (string "minus"),
--      pair (int32 46) (string "period"),
--      pair (int32 47) (string "sol"),
--      pair (int32 58) (string "colon"),
--      pair (int32 59) (string "semi"),
--      pair (int32 60) (string "lt"),
--      pair (int32 61) (string "equals"),
--      pair (int32 62) (string "gt"),
--      pair (int32 63) (string "quest"),
--      pair (int32 64) (string "commat"),
--      pair (int32 91) (string "lsqb"),
--      pair (int32 92) (string "bsol"),
--      pair (int32 93) (string "rsqb"),
--      pair (int32 94) (string "circ"),
--      pair (int32 95) (string "lowbar"),
--      pair (int32 96) (string "grave"),
--      pair (int32 123) (string "lcub"),
--      pair (int32 124) (string "verbar"),
--      pair (int32 125) (string "rcub"),
--      pair (int32 126) (string "tilde")],
--    "capitalize">: lambda "s" $ Strings.toUpper $ var "s",
--    "alias">: lambda "c" $
--      Optionals.maybe
--        (Lists.pure $ var "c")
--        (var "capitalize")
--        (Maps.lookup (Chars.charToInt32 $ var "c") (var "aliases"))]
--    $ Strings.fromList $ Lists.filter Chars.isAlphaNum $ Lists.concat $
--      Lists.map (var "alias") $ Strings.toList $ var "original"
--
--wrapLineDef :: TElement (Int -> String -> String)
--wrapLineDef = formattingDefinition "wrapLine" $
--  doc "A simple soft line wrap which is suitable for code comments" $
--  lambdas ["maxlen", "input"] $ lets [
--    "helper">: lambdas ["prev", "rem"] $
--      Logic.ifElse (Equality.lteInt32 (Strings.length $ var "rem") (var "maxlen"))
--        (Lists.reverse $ Lists.cons (var "rem") (var "prev"))
--        (Logic.ifElse (Lists.null $ var "prefix")
--          (var "helper" @@ (Lists.cons (var "trunc") (var "prev")) @@ (Strings.drop (var "maxlen") (var "rem")))
--          (var "helper" @@ (Lists.cons (Lists.init $ var "prefix") (var "prev")) @@ (var "suffix" ++ (Strings.drop (var "maxlen") (var "rem"))))),
--    "trunc">: Strings.take (var "maxlen") (var "rem"),
--    "spanResult">: Lists.span (lambda "c" $ Logic.and (Logic.not $ Equality.equalChar (var "c") (Chars.char ' ')) (Logic.not $ Equality.equalChar (var "c") (Chars.char '\t'))) (Lists.reverse $ Strings.toList $ var "trunc"),
--    "prefix">: Lists.reverse $ second $ var "spanResult",
--    "suffix">: Strings.fromList $ Lists.reverse $ first $ var "spanResult"]
--    $ Strings.intercalate (string "\n") $ var "helper" @@ (list []) @@ var "input"
