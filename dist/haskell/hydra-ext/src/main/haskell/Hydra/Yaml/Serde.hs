-- Note: this is an automatically generated file. Do not edit.

-- | Native YAML serialization: YAML Node to String

module Hydra.Yaml.Serde where

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Escape single quotes by doubling them
escapeSingleQuotes :: String -> String
escapeSingleQuotes s =

      let squote = 39
      in (Strings.fromList (Lists.bind (Strings.toList s) (\c -> Logic.ifElse (Equality.equal c squote) [
        squote,
        squote] [
        c])))

-- | Check if a string has leading or trailing whitespace
hasLeadingTrailingSpace :: String -> Bool
hasLeadingTrailingSpace s =

      let chars = Strings.toList s
      in (Logic.or (Maybes.fromMaybe False (Maybes.map (\c -> Chars.isSpace c) (Lists.maybeHead chars))) (Maybes.fromMaybe False (Maybes.map (\c -> Chars.isSpace c) (Lists.maybeLast chars))))

-- | Serialize a YAML node to a string
hydraYamlToString :: Model.Node -> String
hydraYamlToString node = writeNode node

-- | Indent all lines of a string by 2 spaces
indentString :: String -> String
indentString s =
    Strings.cat (Lists.map (\line -> Logic.ifElse (Strings.null line) "" (Strings.cat [
      "  ",
      line,
      "\n"])) (Strings.lines s))

-- | Check if character codes represent a decimal number
isDecimalString :: [Int] -> Bool
isDecimalString chars =

      let dotCode = 46
          parts = Lists.span (\c -> Logic.not (Equality.equal c dotCode)) chars
          before = Pairs.first parts
          afterWithDot = Pairs.second parts
      in (Logic.ifElse (Lists.null before) False (Logic.ifElse (Lists.null afterWithDot) False (
        let after = Lists.drop 1 afterWithDot
        in (Logic.ifElse (Lists.null after) False (
          let isDigitFn = \c -> Logic.and (Equality.gte c 48) (Equality.lte c 57)
          in (Logic.and (Lists.null (Lists.filter (\c -> Logic.not (isDigitFn c)) before)) (Lists.null (Lists.filter (\c -> Logic.not (isDigitFn c)) after))))))))

-- | Check if a string looks like a number
looksLikeNumber :: String -> Bool
looksLikeNumber s =

      let chars = Strings.toList s
      in (Maybes.fromMaybe False (Maybes.map (\p ->
        let firstCh = Pairs.first p
            tailCh = Pairs.second p
            rest = Logic.ifElse (Equality.equal firstCh 45) tailCh chars
            isDigitFn = \c -> Logic.and (Equality.gte c 48) (Equality.lte c 57)
            allDigits = Logic.and (Logic.not (Lists.null rest)) (Lists.null (Lists.filter (\c -> Logic.not (isDigitFn c)) rest))
        in (Logic.ifElse allDigits True (isDecimalString rest))) (Lists.uncons chars)))

-- | Check if a string needs quoting in YAML
needsQuoting :: String -> Bool
needsQuoting s =
    Logic.ifElse (Strings.null s) True (Logic.ifElse (Lists.elem s yamlReservedWords) True (Logic.ifElse (looksLikeNumber s) True (
      let chars = Strings.toList s
          specials = Strings.toList yamlSpecialChars
          hasSpecial = Logic.not (Lists.null (Lists.filter (\c -> Lists.elem c specials) chars))
      in (Logic.ifElse hasSpecial True (hasLeadingTrailingSpace s)))))

-- | Write a mapping entry in block style
writeMappingEntry :: (Model.Node, Model.Node) -> String
writeMappingEntry entry =

      let key = Pairs.first entry
          value = Pairs.second entry
      in case value of
        Model.NodeScalar v0 -> Strings.cat [
          writeNodeInline key,
          ": ",
          (writeScalar v0),
          "\n"]
        Model.NodeSequence v0 -> Logic.ifElse (Lists.null v0) (Strings.cat [
          writeNodeInline key,
          ": []\n"]) (Strings.cat [
          writeNodeInline key,
          ":\n",
          (indentString (writeNode value))])
        Model.NodeMapping v0 -> Logic.ifElse (Equality.equal (Maps.size v0) 0) (Strings.cat [
          writeNodeInline key,
          ": {}\n"]) (Strings.cat [
          writeNodeInline key,
          ":\n",
          (indentString (writeNode value))])

-- | Write a mapping entry for the first item of a sequence element
writeMappingEntryInline :: (Model.Node, Model.Node) -> String
writeMappingEntryInline entry =

      let key = Pairs.first entry
          value = Pairs.second entry
      in case value of
        Model.NodeScalar v0 -> Strings.cat [
          writeNodeInline key,
          ": ",
          (writeScalar v0),
          "\n"]
        Model.NodeSequence v0 -> Logic.ifElse (Lists.null v0) (Strings.cat [
          writeNodeInline key,
          ": []\n"]) (Strings.cat [
          writeNodeInline key,
          ":\n",
          (indentString (writeNode value))])
        Model.NodeMapping v0 -> Logic.ifElse (Equality.equal (Maps.size v0) 0) (Strings.cat [
          writeNodeInline key,
          ": {}\n"]) (Strings.cat [
          writeNodeInline key,
          ":\n",
          (indentString (writeNode value))])

-- | Write a YAML node as a top-level value in block style
writeNode :: Model.Node -> String
writeNode node =
    case node of
      Model.NodeScalar v0 -> Strings.cat2 (writeScalar v0) "\n"
      Model.NodeSequence v0 -> Logic.ifElse (Lists.null v0) "[]\n" (Strings.cat (Lists.map (\item -> writeSequenceItem item) v0))
      Model.NodeMapping v0 -> Logic.ifElse (Equality.equal (Maps.size v0) 0) "{}\n" (Strings.cat (Lists.map (\e -> writeMappingEntry e) (Maps.toList v0)))

-- | Write a node inline (for use as a mapping key)
writeNodeInline :: Model.Node -> String
writeNodeInline node =
    case node of
      Model.NodeScalar v0 -> writeScalar v0
      Model.NodeSequence v0 -> Strings.cat [
        "[",
        (Strings.intercalate ", " (Lists.map (\item -> writeNodeInline item) v0)),
        "]"]
      Model.NodeMapping v0 ->
        let writeFlowEntry =
                \e -> Strings.cat [
                  writeNodeInline (Pairs.first e),
                  ": ",
                  (writeNodeInline (Pairs.second e))]
        in (Strings.cat [
          "{",
          (Strings.intercalate ", " (Lists.map writeFlowEntry (Maps.toList v0))),
          "}"])

-- | Write a scalar value
writeScalar :: Model.Scalar -> String
writeScalar s =
    case s of
      Model.ScalarBool v0 -> Logic.ifElse v0 "true" "false"
      Model.ScalarDecimal v0 -> Literals.showDecimal v0
      Model.ScalarFloat v0 -> Literals.showBigfloat v0
      Model.ScalarInt v0 -> Literals.showBigint v0
      Model.ScalarNull -> "null"
      Model.ScalarStr v0 -> writeString v0

-- | Write a sequence item in block style
writeSequenceItem :: Model.Node -> String
writeSequenceItem node =
    case node of
      Model.NodeScalar v0 -> Strings.cat [
        "- ",
        (writeScalar v0),
        "\n"]
      Model.NodeSequence v0 -> Logic.ifElse (Lists.null v0) "- []\n" (Strings.cat2 "-\n" (indentString (writeNode node)))
      Model.NodeMapping v0 -> Logic.ifElse (Equality.equal (Maps.size v0) 0) "- {}\n" (
        let entries = Maps.toList v0
        in (Maybes.fromMaybe "" (Maybes.map (\p ->
          let firstEntry = Pairs.first p
              restEntries = Pairs.second p
              firstStr = writeMappingEntryInline firstEntry
              restStr = Strings.cat (Lists.map (\e -> writeMappingEntry e) restEntries)
          in (Strings.cat [
            "- ",
            firstStr,
            (indentString restStr)])) (Lists.uncons entries))))

-- | Write a string value, quoting if necessary
writeString :: String -> String
writeString s =
    Logic.ifElse (needsQuoting s) (Strings.cat [
      "'",
      (escapeSingleQuotes s),
      "'"]) s

-- | YAML reserved words that need quoting
yamlReservedWords :: [String]
yamlReservedWords =
    [
      "true",
      "false",
      "null",
      "~",
      "yes",
      "no",
      "on",
      "off",
      "True",
      "False",
      "Null",
      "Yes",
      "No",
      "On",
      "Off",
      "TRUE",
      "FALSE",
      "NULL",
      "YES",
      "NO",
      "ON",
      "OFF"]

-- | YAML special characters that trigger quoting
yamlSpecialChars :: String
yamlSpecialChars = ": {}[]#,&*!|>'\"%@`"
