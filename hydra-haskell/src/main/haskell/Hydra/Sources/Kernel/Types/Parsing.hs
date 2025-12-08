module Hydra.Sources.Kernel.Types.Parsing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.parsing"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "Parser combinator types for text parsing"
  where
    elements = [
      parseError,
      parseResult,
      parseSuccess,
      parser]

parseError :: Binding
parseError = define "ParseError" $
  doc "An error which occurred while parsing" $
  T.record [
    "message">:
      doc "An error message" $
      T.string,
    "remainder">:
      doc "The remaining input at the point of failure" $
      T.string]

parseResult :: Binding
parseResult = define "ParseResult" $
  doc "The result of a parse operation" $
  T.forAll "a" $ T.union [
    "success">:
      doc "A successful parse, with a value and the remaining unparsed input" $
      parseSuccess @@ "a",
    "failure">:
      doc "A failed parse, with an error message and the remaining input"
      parseError]

parseSuccess :: Binding
parseSuccess = define "ParseSuccess" $
  doc "A successful parse result" $
  T.forAll "a" $ T.record [
    "value">:
      doc "The parsed value"
      "a",
    "remainder">:
      doc "The remaining unparsed input" $
      T.string]

parser :: Binding
parser = define "Parser" $
  doc "A parser which consumes characters from a string and produces a value" $
  T.forAll "a" $ T.wrap $
    T.string ~> parseResult @@ "a"
