module Hydra.Sources.Kernel.Types.Parsing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Core.Overlay.Haskell.Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.core.parsing"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "Parser combinator types for text parsing")}
  where
    definitions = [
      parseError,
      parseInput,
      parseResult,
      parseSuccess,
      parser,
      sourcePosition,
      sourceSpan]

parseError :: TypeDefinition
parseError = define "ParseError" $
  doc "An error which occurred while parsing" $
  T.record [
    "message">:
      doc "An error message" $
      T.string,
    "remainder">:
      doc "The remaining input at the point of failure" $
      parseInput]

parseInput :: TypeDefinition
parseInput = define "ParseInput" $
  doc ("The input state threaded through a parser: the unconsumed codepoints, together with the number of"
    <> " codepoints already consumed, and where the last token ended. The offset makes the absolute source position of any parse step available"
    <> " in constant time, without rescanning the input.") $
  T.record [
    "offset">:
      doc "The number of codepoints consumed so far, i.e. the absolute offset of the first remaining codepoint" $
      T.int32,
    "tokenEnd">:
      doc ("The offset just past the most recently completed token, excluding any whitespace or comments that"
        <> " followed it. Used to give source spans a tight end") $
      T.int32,
    "codepoints">:
      doc ("The remaining unparsed input, as codepoints. Represented as a list rather than a string, so that"
        <> " consuming one character is a constant-time operation") $
      T.list T.int32]

parseResult :: TypeDefinition
parseResult = define "ParseResult" $
  doc "The result of a parse operation" $
  T.forAll "a" $ T.union [
    "success">:
      doc "A successful parse, with a value and the remaining unparsed input" $
      parseSuccess @@ "a",
    "failure">:
      doc "A failed parse, with an error message and the remaining input"
      parseError]

parseSuccess :: TypeDefinition
parseSuccess = define "ParseSuccess" $
  doc "A successful parse result" $
  T.forAll "a" $ T.record [
    "value">:
      doc "The parsed value"
      "a",
    "remainder">:
      doc "The remaining unparsed input" $
      parseInput]

parser :: TypeDefinition
parser = define "Parser" $
  doc ("A parser which consumes characters from a codepoint list and produces a value."
    <> " The input is a list, rather than a string, so that consuming one character is a"
    <> " constant-time operation rather than a linear-time string copy.") $
  T.forAll "a" $ T.wrap $
    parseInput ~> parseResult @@ "a"

sourcePosition :: TypeDefinition
sourcePosition = define "SourcePosition" $
  doc ("A position in source text, as an editor reports it: zero-based line and zero-based column."
    <> " The column counts UTF-16 code units, matching the Language Server Protocol's Position.") $
  T.record [
    "line">:
      doc "The zero-based line number. Lines are terminated by LF, CR, or CR LF" $
      T.int32,
    "column">:
      doc "The zero-based column, in UTF-16 code units from the start of the line" $
      T.int32]

sourceSpan :: TypeDefinition
sourceSpan = define "SourceSpan" $
  doc ("A half-open range [start, end) of source text, measured in codepoints from the start of the input."
    <> " Attached to parsed terms and types as a reserved, round-trip-exempt annotation (see hydra.core.positions).") $
  T.record [
    "start">:
      doc "The offset of the first codepoint of the span" $
      T.int32,
    "end">:
      doc "The offset one past the last codepoint of the span" $
      T.int32]
