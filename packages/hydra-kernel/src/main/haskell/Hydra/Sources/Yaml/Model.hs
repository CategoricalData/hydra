-- | A basic YAML representation model

module Hydra.Sources.Yaml.Model where

-- Standard type-level imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.yaml.model"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns] [Core.ns] $
    Just ("A basic YAML representation model. Based on:\n" ++
      "  https://yaml.org/spec/1.2/spec.html\n" ++
      "The Serialization and Presentation properties of YAML,\n" ++
      "including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.\n" ++
      "In addition, tags are omitted from this model, and non-standard scalars are unsupported.")
  where
    definitions = [
      node,
      scalar]

-- Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
node :: Binding
node = define "Node" $
  doc "A YAML node (value)" $
  T.union [
    "mapping">:
      doc "A mapping from nodes to nodes" $
      T.map node node, -- Failsafe schema: tag:yaml.org,2002:map
    "scalar">:
      doc "A scalar value"
      scalar,
    "sequence">:
      doc "A sequence of nodes" $
      T.list node] -- Failsafe schema: tag:yaml.org,2002:seq

scalar :: Binding
scalar = define "Scalar" $
  doc "A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here" $
  T.union [
    -- Represents a true/false value
    -- JSON schema: tag:yaml.org,2002:bool
    "bool">:
      doc "Represents a true/false value"
      T.boolean,
    -- An arbitrary-precision decimal number, encoded as a plain scalar whose lexical form is a
    -- valid JSON number. Under the YAML 1.2.2 core schema this resolves as !!float, but Hydra
    -- preserves full source precision on its side of the wire (the YAML spec leaves the value
    -- space of !!float implementation-defined). Used by the JSON<->YAML bridge so JSON numbers,
    -- which are decimal-encoded by spec, survive the trip losslessly. Like "float", the
    -- decimal scalar has no NaN or infinity value in Hydra YAML.
    "decimal">:
      doc "An arbitrary-precision decimal number (lexically a valid JSON number)"
      T.decimal,
    -- Represents an approximation to real numbers
    -- JSON schema: tag:yaml.org,2002:float
    -- In addition to arbitrary-precision floating-point numbers in scientific notation,
    -- YAML allows for three special values, which are not supported here:
    -- positive and negative infinity (.inf and -.inf), and "not a number (.nan)
    "float">:
      doc "Represents an approximation to real numbers"
      T.bigfloat,
    -- Represents arbitrary sized finite mathematical integers
    -- JSON schema: tag:yaml.org,2002:int
    "int">:
      doc "Represents arbitrary sized finite mathematical integers"
      T.bigint,
    -- Represents the lack of a value
    -- JSON schema: tag:yaml.org,2002:null
    "null">:
      doc "Represents the lack of a value"
      T.unit,
    -- Failsafe schema: tag:yaml.org,2002:str
    "str">:
      doc "A string value"
      T.string]
