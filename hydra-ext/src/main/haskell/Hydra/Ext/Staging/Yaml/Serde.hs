-- | Native YAML serialization and deserialization (no external YAML library dependency)

module Hydra.Ext.Staging.Yaml.Serde where

import qualified Hydra.Ext.Org.Yaml.Model as YM

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M


-- ========================================
-- Writer: YAML Node -> String
-- ========================================

-- | Serialize a YAML node to a string
hydraYamlToString :: YM.Node -> String
hydraYamlToString node = writeNode node

-- | Serialize a YAML node to a lazy ByteString
hydraYamlToBytes :: YM.Node -> BS.ByteString
hydraYamlToBytes = LB.pack . hydraYamlToString

-- | Write a YAML node as a top-level value (block style)
writeNode :: YM.Node -> String
writeNode node = case node of
  YM.NodeScalar s -> writeScalar s ++ "\n"
  YM.NodeSequence [] -> "[]\n"
  YM.NodeSequence items -> concatMap writeSequenceItem items
  YM.NodeMapping m
    | M.null m -> "{}\n"
    | otherwise -> concatMap writeMappingEntry (M.toList m)

-- | Write a scalar value
writeScalar :: YM.Scalar -> String
writeScalar s = case s of
  YM.ScalarBool b -> if b then "true" else "false"
  YM.ScalarFloat f -> show f
  YM.ScalarInt i -> show i
  YM.ScalarNull -> "null"
  YM.ScalarStr str -> writeString str

-- | Write a string value, quoting if necessary
writeString :: String -> String
writeString s
  | needsQuoting s = "'" ++ escapeSingleQuotes s ++ "'"
  | otherwise = s
  where
    needsQuoting "" = True
    needsQuoting str =
      str `elem` ["true", "false", "null", "~", "yes", "no", "on", "off",
                   "True", "False", "Null", "Yes", "No", "On", "Off",
                   "TRUE", "FALSE", "NULL", "YES", "NO", "ON", "OFF"]
      || looksLikeNumber str
      || any (`elem` str) ": {}[]#,&*!|>'\"%@`"
      || hasLeadingTrailingSpace str

    looksLikeNumber ('-':rest) = looksLikeNumber rest
    looksLikeNumber str' = case str' of
      [] -> False
      _ -> all C.isDigit str' || isDecimal str'

    isDecimal str' = case break (== '.') str' of
      (before, '.':after) -> not (null before) && not (null after) && all C.isDigit before && all C.isDigit after
      _ -> False

    hasLeadingTrailingSpace str' = not (null str') && (C.isSpace (head str') || C.isSpace (last str'))

    escapeSingleQuotes [] = []
    escapeSingleQuotes ('\'':rest) = '\'' : '\'' : escapeSingleQuotes rest
    escapeSingleQuotes (c:rest) = c : escapeSingleQuotes rest

-- | Write a sequence item in block style
writeSequenceItem :: YM.Node -> String
writeSequenceItem node = case node of
  YM.NodeScalar s -> "- " ++ writeScalar s ++ "\n"
  YM.NodeSequence [] -> "- []\n"
  YM.NodeSequence _ -> "-\n" ++ indentString (writeNode node)
  YM.NodeMapping m
    | M.null m -> "- {}\n"
    | otherwise ->
        let entries = M.toList m
            firstEntry = head entries
            restEntries = tail entries
            firstStr = writeMappingEntryInline firstEntry
            restStr = concatMap writeMappingEntry restEntries
        in "- " ++ firstStr ++ indentString restStr

-- | Write a mapping entry in block style (key: value\n)
writeMappingEntry :: (YM.Node, YM.Node) -> String
writeMappingEntry (key, value) = case value of
  YM.NodeScalar s -> writeNodeInline key ++ ": " ++ writeScalar s ++ "\n"
  YM.NodeSequence [] -> writeNodeInline key ++ ": []\n"
  YM.NodeSequence _ -> writeNodeInline key ++ ":\n" ++ indentString (writeNode value)
  YM.NodeMapping m
    | M.null m -> writeNodeInline key ++ ": {}\n"
    | otherwise -> writeNodeInline key ++ ":\n" ++ indentString (writeNode value)

-- | Write a mapping entry for the first item of a sequence element
writeMappingEntryInline :: (YM.Node, YM.Node) -> String
writeMappingEntryInline (key, value) = case value of
  YM.NodeScalar s -> writeNodeInline key ++ ": " ++ writeScalar s ++ "\n"
  YM.NodeSequence [] -> writeNodeInline key ++ ": []\n"
  YM.NodeSequence _ -> writeNodeInline key ++ ":\n" ++ indentString (writeNode value)
  YM.NodeMapping m
    | M.null m -> writeNodeInline key ++ ": {}\n"
    | otherwise -> writeNodeInline key ++ ":\n" ++ indentString (writeNode value)

-- | Write a node inline (for use as a mapping key)
writeNodeInline :: YM.Node -> String
writeNodeInline node = case node of
  YM.NodeScalar s -> writeScalar s
  YM.NodeSequence items -> "[" ++ L.intercalate ", " (map writeNodeInline items) ++ "]"
  YM.NodeMapping m -> "{" ++ L.intercalate ", " (map writeFlowEntry (M.toList m)) ++ "}"
  where
    writeFlowEntry (k, v) = writeNodeInline k ++ ": " ++ writeNodeInline v

-- | Indent all lines of a string by 2 spaces
indentString :: String -> String
indentString = concatMap indentLine . lines
  where
    indentLine "" = ""
    indentLine line = "  " ++ line ++ "\n"


-- ========================================
-- Parser: String -> YAML Node
-- ========================================

-- | Parse a YAML string into a Hydra YAML node
parseYamlString :: String -> Either String YM.Node
parseYamlString input = case parseBlock (removeTrailingNewlines input) 0 of
  Left err -> Left err
  Right (node, _) -> Right node

-- | Parse a YAML byte string into a Hydra YAML node
bytesToHydraYaml :: BS.ByteString -> Either String YM.Node
bytesToHydraYaml = parseYamlString . LB.unpack

-- | Remove trailing newlines
removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (== '\n') . reverse

-- | Parse a block-level value at a given indentation
parseBlock :: String -> Int -> Either String (YM.Node, String)
parseBlock input indent = case dropInlineWhitespace input of
  -- Empty sequence
  '[':']':rest -> Right (YM.NodeSequence [], rest)
  -- Empty mapping
  '{':'}':rest -> Right (YM.NodeMapping M.empty, rest)
  -- Block sequence
  '-':' ':_ -> parseBlockSequence input indent
  '-':'\n':_ -> parseBlockSequence input indent
  -- Try scalar first
  _ -> case parseScalarValue input of
    Just (scalar, rest) -> Right (YM.NodeScalar scalar, rest)
    Nothing -> case parseBlockMapping input indent of
      Right result -> Right result
      Left _ -> Left $ "YAML parse error at: " ++ take 40 input

-- | Parse a block sequence
parseBlockSequence :: String -> Int -> Either String (YM.Node, String)
parseBlockSequence input indent = go input []
  where
    go s acc = case stripIndent s indent of
      Just rest -> case rest of
        '-':' ':itemRest -> do
          (node, remaining) <- parseSeqItem itemRest (indent + 2)
          go remaining (acc ++ [node])
        '-':'\n':itemRest -> do
          (node, remaining) <- parseBlock (skipToNextLine itemRest) (indent + 2)
          go (dropWhile (== '\n') remaining) (acc ++ [node])
        _ -> Right (YM.NodeSequence acc, s)
      Nothing
        | null acc -> Left $ "expected sequence item at: " ++ take 40 s
        | otherwise -> Right (YM.NodeSequence acc, s)

    parseSeqItem s itemIndent = case s of
      -- Inline scalar
      _ | Just (scalar, rest) <- parseScalarValue s ->
          Right (YM.NodeScalar scalar, dropNewline rest)
      -- Nested block
      _ -> parseBlock s itemIndent

-- | Parse a block mapping
parseBlockMapping :: String -> Int -> Either String (YM.Node, String)
parseBlockMapping input indent = go input []
  where
    go s acc = case stripIndent s indent of
      Just rest -> case parseKeyColon rest of
        Just (keyStr, afterColon) ->
          let key = YM.NodeScalar (YM.ScalarStr keyStr)
          in case dropInlineWhitespace afterColon of
            -- Value on next line (block)
            '\n':nextLine -> do
              let valueIndent = detectIndent nextLine
              if valueIndent > indent
                then do
                  (value, remaining) <- parseBlock nextLine valueIndent
                  go remaining (acc ++ [(key, value)])
                else do
                  -- Empty value = null
                  go nextLine (acc ++ [(key, YM.NodeScalar YM.ScalarNull)])
            -- Inline value
            valueStr -> do
              (value, remaining) <- parseInlineValue valueStr
              go remaining (acc ++ [(key, value)])
        Nothing
          | null acc -> Left $ "expected mapping key at: " ++ take 40 s
          | otherwise -> Right (YM.NodeMapping (M.fromList acc), s)
      Nothing
        | null acc -> Left $ "expected mapping at indent " ++ show indent ++ ": " ++ take 40 s
        | otherwise -> Right (YM.NodeMapping (M.fromList acc), s)

-- | Parse a key followed by a colon
parseKeyColon :: String -> Maybe (String, String)
parseKeyColon input =
  let (key, rest) = break (\c -> c == ':' || c == '\n') input
  in case rest of
    ':':' ':after -> if null key then Nothing else Just (key, after)
    ':':'\n':after -> if null key then Nothing else Just (key, '\n':after)
    ':':[] -> if null key then Nothing else Just (key, "")
    _ -> Nothing

-- | Parse a scalar value at the current position
parseScalarValue :: String -> Maybe (YM.Scalar, String)
parseScalarValue input =
  let s = dropInlineWhitespace input
      (word, rest) = breakScalar s
  in case word of
    "null" -> Just (YM.ScalarNull, rest)
    "~" -> Just (YM.ScalarNull, rest)
    "true" -> Just (YM.ScalarBool True, rest)
    "false" -> Just (YM.ScalarBool False, rest)
    _ | Just n <- tryParseInt word -> Just (YM.ScalarInt n, rest)
    _ | Just f <- tryParseFloat word -> Just (YM.ScalarFloat f, rest)
    _ | not (null word) -> Just (YM.ScalarStr word, rest)
    _ -> Nothing

-- | Parse a value inline (for mapping values on the same line as the key)
parseInlineValue :: String -> Either String (YM.Node, String)
parseInlineValue input = case dropInlineWhitespace input of
  '[':']':rest -> Right (YM.NodeSequence [], dropNewline rest)
  '{':'}':rest -> Right (YM.NodeMapping M.empty, dropNewline rest)
  s -> case parseScalarValue s of
    Just (scalar, rest) -> Right (YM.NodeScalar scalar, dropNewline rest)
    Nothing -> Left $ "could not parse inline value: " ++ take 40 s

-- | Break input at the end of a scalar value
breakScalar :: String -> (String, String)
breakScalar s = case s of
  '\'' : rest -> let (content, afterQuote) = breakSingleQuoted rest
                 in (content, afterQuote)
  '"' : rest -> let (content, afterQuote) = breakDoubleQuoted rest
                in (content, afterQuote)
  _ -> let (word, rest') = break (\c -> c == '\n' || c == '\r') s
       in (trimTrailingSpaces word, rest')

-- | Break a single-quoted string
breakSingleQuoted :: String -> (String, String)
breakSingleQuoted [] = ("", "")
breakSingleQuoted ('\'':'\'':rest) = let (s, r) = breakSingleQuoted rest in ('\'':s, r)
breakSingleQuoted ('\'':rest) = ("", rest)
breakSingleQuoted (c:rest) = let (s, r) = breakSingleQuoted rest in (c:s, r)

-- | Break a double-quoted string
breakDoubleQuoted :: String -> (String, String)
breakDoubleQuoted [] = ("", "")
breakDoubleQuoted ('\\':c:rest) = let (s, r) = breakDoubleQuoted rest in ('\\':c:s, r)
breakDoubleQuoted ('"':rest) = ("", rest)
breakDoubleQuoted (c:rest) = let (s, r) = breakDoubleQuoted rest in (c:s, r)

-- | Try to parse a string as an integer
tryParseInt :: String -> Maybe Integer
tryParseInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Try to parse a string as a float (only if it contains a dot or exponent)
tryParseFloat :: String -> Maybe Double
tryParseFloat s
  | any (`elem` s) ".eE" = case reads s of
      [(f, "")] -> Just f
      _ -> Nothing
  | otherwise = Nothing

-- | Strip exactly n spaces of indentation, returning the rest if possible
stripIndent :: String -> Int -> Maybe String
stripIndent s 0 = Just s
stripIndent [] _ = Nothing
stripIndent ('\n':rest) n = stripIndent rest n  -- skip blank lines
stripIndent (' ':rest) n = stripIndent rest (n - 1)
stripIndent _ _ = Nothing

-- | Detect the indentation level of the next non-empty line
detectIndent :: String -> Int
detectIndent = length . takeWhile (== ' ')

-- | Drop inline whitespace (spaces and tabs)
dropInlineWhitespace :: String -> String
dropInlineWhitespace = dropWhile (\c -> c == ' ' || c == '\t')

-- | Drop a single newline if present
dropNewline :: String -> String
dropNewline ('\r':'\n':rest) = rest
dropNewline ('\n':rest) = rest
dropNewline s = s

-- | Skip to the start of the next line
skipToNextLine :: String -> String
skipToNextLine = dropWhile (/= '\n') . dropWhile (== '\n')

-- | Trim trailing spaces
trimTrailingSpaces :: String -> String
trimTrailingSpaces = reverse . dropWhile (== ' ') . reverse


