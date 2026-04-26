
module Hydra.Sources.Yaml.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

import qualified Hydra.Yaml.Model as YM


ns :: Namespace
ns = Namespace "hydra.yaml.serde"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    []
    (KernelTypes.kernelTypesNamespaces L.++ [Namespace "hydra.yaml.model"]) $
    Just "Native YAML serialization: YAML Node to String"
  where
    definitions = [
      toDefinition escapeSingleQuotes,
      toDefinition hasLeadingTrailingSpace,
      toDefinition hydraYamlToString,
      toDefinition indentString,
      toDefinition isDecimalString,
      toDefinition looksLikeNumber,
      toDefinition needsQuoting,
      toDefinition writeMappingEntry,
      toDefinition writeMappingEntryInline,
      toDefinition writeNode,
      toDefinition writeNodeInline,
      toDefinition writeScalar,
      toDefinition writeSequenceItem,
      toDefinition writeString,
      toDefinition yamlReservedWords,
      toDefinition yamlSpecialChars]

-- | Escape single quotes by doubling them
escapeSingleQuotes :: TTermDefinition (String -> String)
escapeSingleQuotes = define "escapeSingleQuotes" $
  doc "Escape single quotes by doubling them" $
  "s" ~>
  "squote" <~ int32 39 $  -- '\''
  Strings.fromList $ Lists.bind (Strings.toList $ var "s")
    ("c" ~> Logic.ifElse (Equality.equal (var "c") (var "squote"))
      (list [var "squote", var "squote"])
      (list [var "c"]))

-- | Check if a string has leading or trailing whitespace
hasLeadingTrailingSpace :: TTermDefinition (String -> Bool)
hasLeadingTrailingSpace = define "hasLeadingTrailingSpace" $
  doc "Check if a string has leading or trailing whitespace" $
  "s" ~>
  "chars" <~ Strings.toList (var "s") $
  Logic.or
    (Maybes.fromMaybe false (Maybes.map (lambda "c" $ Chars.isSpace (var "c")) (Lists.maybeHead (var "chars"))))
    (Maybes.fromMaybe false (Maybes.map (lambda "c" $ Chars.isSpace (var "c")) (Lists.maybeLast (var "chars"))))

-- | Serialize a YAML node to a string
hydraYamlToString :: TTermDefinition (YM.Node -> String)
hydraYamlToString = define "hydraYamlToString" $
  doc "Serialize a YAML node to a string" $
  lambda "node" $ writeNode @@ var "node"

-- | Indent all lines of a string by 2 spaces
indentString :: TTermDefinition (String -> String)
indentString = define "indentString" $
  doc "Indent all lines of a string by 2 spaces" $
  "s" ~>
  Strings.cat $ Lists.map
    ("line" ~> Logic.ifElse (Strings.null $ var "line")
      (string "")
      (Strings.cat $ list [string "  ", var "line", string "\n"]))
    (Strings.lines $ var "s")

-- | Check if a list of character codes represents a decimal number (digits.digits)
isDecimalString :: TTermDefinition ([Int] -> Bool)
isDecimalString = define "isDecimalString" $
  doc "Check if character codes represent a decimal number" $
  "chars" ~>
  "dotCode" <~ int32 46 $  -- '.'
  "parts" <~ Lists.span ("c" ~> Logic.not (Equality.equal (var "c") (var "dotCode"))) (var "chars") $
  "before" <~ Pairs.first (var "parts") $
  "afterWithDot" <~ Pairs.second (var "parts") $
  -- Must have something before the dot
  Logic.ifElse (Lists.null $ var "before") false $
  -- Must have the dot
  Logic.ifElse (Lists.null $ var "afterWithDot") false $
  -- Drop the dot
  "after" <~ Lists.drop (int32 1) (var "afterWithDot") $
  -- Must have something after the dot
  Logic.ifElse (Lists.null $ var "after") false $
  -- Both parts must be all digits
  "isDigitFn" <~ ("c" ~> Logic.and
    (Equality.gte (var "c") (int32 48))
    (Equality.lte (var "c") (int32 57))) $
  Logic.and
    (Lists.null (Lists.filter ("c" ~> Logic.not (var "isDigitFn" @@ var "c")) (var "before")))
    (Lists.null (Lists.filter ("c" ~> Logic.not (var "isDigitFn" @@ var "c")) (var "after")))

-- | Check if a string looks like a number
looksLikeNumber :: TTermDefinition (String -> Bool)
looksLikeNumber = define "looksLikeNumber" $
  doc "Check if a string looks like a number" $
  "s" ~>
  "chars" <~ Strings.toList (var "s") $
  Maybes.fromMaybe false $ Maybes.map
    (lambda "p" $ lets [
      "firstCh">: Pairs.first (var "p"),
      "tailCh">: Pairs.second (var "p"),
      -- Handle leading minus
      "rest">: Logic.ifElse (Equality.equal (var "firstCh") (int32 45))  -- '-'
        (var "tailCh")
        (var "chars"),
      "isDigitFn">: "c" ~> Logic.and
        (Equality.gte (var "c") (int32 48))   -- '0'
        (Equality.lte (var "c") (int32 57)),  -- '9'
      "allDigits">: Logic.and
        (Logic.not (Lists.null (var "rest")))
        (Lists.null (Lists.filter
          ("c" ~> Logic.not (var "isDigitFn" @@ var "c"))
          (var "rest")))] $
      Logic.ifElse (var "allDigits") true $
      -- Decimal?
      isDecimalString @@ var "rest")
    (Lists.uncons (var "chars"))

-- | Check if a string needs quoting in YAML
needsQuoting :: TTermDefinition (String -> Bool)
needsQuoting = define "needsQuoting" $
  doc "Check if a string needs quoting in YAML" $
  "s" ~>
  -- Empty string needs quoting
  Logic.ifElse (Strings.null $ var "s") true $
  -- Reserved words need quoting
  Logic.ifElse (Lists.elem (var "s") (var "hydra.yaml.serde.yamlReservedWords" :: TTerm [String])) true $
  -- Looks like a number needs quoting
  Logic.ifElse (looksLikeNumber @@ var "s") true $
  -- Contains special characters needs quoting
  "chars" <~ Strings.toList (var "s") $
  "specials" <~ Strings.toList (var "hydra.yaml.serde.yamlSpecialChars" :: TTerm String) $
  "hasSpecial" <~ Logic.not (Lists.null (Lists.filter
    ("c" ~> Lists.elem (var "c" :: TTerm Int) (var "specials"))
    (var "chars"))) $
  Logic.ifElse (var "hasSpecial") true $
  -- Leading or trailing space needs quoting
  hasLeadingTrailingSpace @@ var "s"

-- | Write a mapping entry in block style (key: value\n)
writeMappingEntry :: TTermDefinition ((YM.Node, YM.Node) -> String)
writeMappingEntry = define "writeMappingEntry" $
  doc "Write a mapping entry in block style" $
  "entry" ~>
  "key" <~ Pairs.first (var "entry") $
  "value" <~ Pairs.second (var "entry") $
  cases YM._Node (var "value") Nothing [
    YM._Node_scalar>>: "s" ~> Strings.cat $ list [writeNodeInline @@ var "key", string ": ", writeScalar @@ var "s", string "\n"],
    YM._Node_sequence>>: "items" ~>
      Logic.ifElse (Lists.null $ var "items")
        (Strings.cat $ list [writeNodeInline @@ var "key", string ": []\n"])
        (Strings.cat $ list [writeNodeInline @@ var "key", string ":\n", indentString @@ (writeNode @@ var "value")]),
    YM._Node_mapping>>: "m" ~>
      Logic.ifElse (Equality.equal (Maps.size (var "m")) (int32 0))
        (Strings.cat $ list [writeNodeInline @@ var "key", string ": {}\n"])
        (Strings.cat $ list [writeNodeInline @@ var "key", string ":\n", indentString @@ (writeNode @@ var "value")])]

-- | Write a mapping entry for the first item of a sequence element
writeMappingEntryInline :: TTermDefinition ((YM.Node, YM.Node) -> String)
writeMappingEntryInline = define "writeMappingEntryInline" $
  doc "Write a mapping entry for the first item of a sequence element" $
  "entry" ~>
  "key" <~ Pairs.first (var "entry") $
  "value" <~ Pairs.second (var "entry") $
  cases YM._Node (var "value") Nothing [
    YM._Node_scalar>>: "s" ~> Strings.cat $ list [writeNodeInline @@ var "key", string ": ", writeScalar @@ var "s", string "\n"],
    YM._Node_sequence>>: "items" ~>
      Logic.ifElse (Lists.null $ var "items")
        (Strings.cat $ list [writeNodeInline @@ var "key", string ": []\n"])
        (Strings.cat $ list [writeNodeInline @@ var "key", string ":\n", indentString @@ (writeNode @@ var "value")]),
    YM._Node_mapping>>: "m" ~>
      Logic.ifElse (Equality.equal (Maps.size (var "m")) (int32 0))
        (Strings.cat $ list [writeNodeInline @@ var "key", string ": {}\n"])
        (Strings.cat $ list [writeNodeInline @@ var "key", string ":\n", indentString @@ (writeNode @@ var "value")])]

-- | Write a YAML node as a top-level value (block style)
writeNode :: TTermDefinition (YM.Node -> String)
writeNode = define "writeNode" $
  doc "Write a YAML node as a top-level value in block style" $
  "node" ~> cases YM._Node (var "node") Nothing [
    YM._Node_scalar>>: "s" ~> Strings.cat2 (writeScalar @@ var "s") (string "\n"),
    YM._Node_sequence>>: "items" ~>
      Logic.ifElse (Lists.null $ var "items")
        (string "[]\n")
        (Strings.cat $ Lists.map (lambda "item" $ writeSequenceItem @@ var "item") (var "items")),
    YM._Node_mapping>>: "m" ~>
      Logic.ifElse (Equality.equal (Maps.size (var "m")) (int32 0))
        (string "{}\n")
        (Strings.cat $ Lists.map (lambda "e" $ writeMappingEntry @@ var "e") (Maps.toList $ var "m"))]

-- | Write a node inline (for use as a mapping key)
writeNodeInline :: TTermDefinition (YM.Node -> String)
writeNodeInline = define "writeNodeInline" $
  doc "Write a node inline (for use as a mapping key)" $
  "node" ~> cases YM._Node (var "node") Nothing [
    YM._Node_scalar>>: "s" ~> writeScalar @@ var "s",
    YM._Node_sequence>>: "items" ~>
      Strings.cat $ list [
        string "[",
        Strings.intercalate (string ", ") (Lists.map (lambda "item" $ writeNodeInline @@ var "item") (var "items")),
        string "]"],
    YM._Node_mapping>>: "m" ~>
      "writeFlowEntry" <~ ("e" ~>
        Strings.cat $ list [
          writeNodeInline @@ (Pairs.first $ var "e"),
          string ": ",
          writeNodeInline @@ (Pairs.second $ var "e")]) $
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") (Lists.map (var "writeFlowEntry") (Maps.toList $ var "m")),
        string "}"]]

-- | Write a scalar value
writeScalar :: TTermDefinition (YM.Scalar -> String)
writeScalar = define "writeScalar" $
  doc "Write a scalar value" $
  "s" ~> cases YM._Scalar (var "s") Nothing [
    YM._Scalar_bool>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
    YM._Scalar_decimal>>: "d" ~> Literals.showDecimal (var "d"),
    YM._Scalar_float>>: "f" ~> Literals.showBigfloat (var "f"),
    YM._Scalar_int>>: "i" ~> Literals.showBigint (var "i"),
    YM._Scalar_null>>: constant (string "null"),
    YM._Scalar_str>>: "str" ~> writeString @@ var "str"]

-- | Write a sequence item in block style
writeSequenceItem :: TTermDefinition (YM.Node -> String)
writeSequenceItem = define "writeSequenceItem" $
  doc "Write a sequence item in block style" $
  "node" ~> cases YM._Node (var "node") Nothing [
    YM._Node_scalar>>: "s" ~> Strings.cat $ list [string "- ", writeScalar @@ var "s", string "\n"],
    YM._Node_sequence>>: "items" ~>
      Logic.ifElse (Lists.null $ var "items")
        (string "- []\n")
        (Strings.cat2 (string "-\n") (indentString @@ (writeNode @@ var "node"))),
    YM._Node_mapping>>: "m" ~>
      Logic.ifElse (Equality.equal (Maps.size (var "m")) (int32 0))
        (string "- {}\n")
        ("entries" <~ Maps.toList (var "m") $
         Maybes.fromMaybe (string "") $ Maybes.map
           (lambda "p" $ lets [
             "firstEntry">: Pairs.first (var "p"),
             "restEntries">: Pairs.second (var "p"),
             "firstStr">: writeMappingEntryInline @@ var "firstEntry",
             "restStr">: Strings.cat $ Lists.map (lambda "e" $ writeMappingEntry @@ var "e") (var "restEntries")] $
             Strings.cat $ list [string "- ", var "firstStr", indentString @@ var "restStr"])
           (Lists.uncons (var "entries")))]

-- | Write a string value, quoting if necessary
writeString :: TTermDefinition (String -> String)
writeString = define "writeString" $
  doc "Write a string value, quoting if necessary" $
  "s" ~>
  Logic.ifElse (needsQuoting @@ var "s")
    (Strings.cat $ list [string "'", escapeSingleQuotes @@ var "s", string "'"])
    (var "s")

-- | YAML reserved words that need quoting
yamlReservedWords :: TTermDefinition [String]
yamlReservedWords = define "yamlReservedWords" $
  doc "YAML reserved words that need quoting" $
  list [
    string "true", string "false", string "null", string "~",
    string "yes", string "no", string "on", string "off",
    string "True", string "False", string "Null",
    string "Yes", string "No", string "On", string "Off",
    string "TRUE", string "FALSE", string "NULL",
    string "YES", string "NO", string "ON", string "OFF"]

-- | YAML special characters that trigger quoting
yamlSpecialChars :: TTermDefinition String
yamlSpecialChars = define "yamlSpecialChars" $
  doc "YAML special characters that trigger quoting" $
  string ": {}[]#,&*!|>'\"%@`"
