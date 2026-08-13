module Hydra.Sources.Kernel.Types.Parsing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.parsing"

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
      parseResult,
      parseSuccess,
      parser]

parseError :: TypeDefinition
parseError = define "ParseError" $
  doc "An error which occurred while parsing" $
  T.record [
    "message">:
      doc "An error message" $
      T.string,
    "remainder">:
      doc "The remaining input (as codepoints) at the point of failure" $
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
      doc "The remaining unparsed input, as codepoints. Represented as a list rather than a string" $
      T.list T.int32]

parser :: TypeDefinition
parser = define "Parser" $
  doc ("A parser which consumes characters from a codepoint list and produces a value."
    <> " The input is a list, rather than a string, so that consuming one character is a"
    <> " constant-time operation rather than a linear-time string copy.") $
  T.forAll "a" $ T.wrap $
    T.list T.int32 ~> parseResult @@ "a"
