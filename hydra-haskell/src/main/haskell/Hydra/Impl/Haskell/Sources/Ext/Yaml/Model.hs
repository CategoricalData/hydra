{-
A basic YAML representation model

Based on https://yaml.org/spec/1.2/spec.html

The Serialization and Presentation properties of YAML,
including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
In addition, tags are omitted from this model, and non-standard scalars are unsupported.
-}

module Hydra.Impl.Haskell.Sources.Ext.Yaml.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


yamlModelModule :: Module Meta
yamlModelModule = Module yamlModel []

yamlModelName :: GraphName
yamlModelName = GraphName "hydra/ext/yaml/model"

yamlModel :: Graph Meta
yamlModel = Graph yamlModelName elements hydraCoreName
  where
    def = datatype coreContext yamlModelName
    model = nsref yamlModelName

    elements = [

      {-
      Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
      -}
      def "Node" $
        doc "A YAML node (value)" $
        union [
          "mapping">: Types.map (model "Node") (model "Node"), -- Failsafe schema: tag:yaml.org,2002:map
          "scalar">: model "Scalar",
          "sequence">: list $ model "Node"], -- Failsafe schema: tag:yaml.org,2002:seq

      def "Scalar" $
        doc "A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here" $
        union [
          {-
          Represents a true/false value

          JSON schema: tag:yaml.org,2002:bool
          -}
          "bool">:
            doc "Represents a true/false value"
            boolean,
          {-
          Represents an approximation to real numbers

          JSON schema: tag:yaml.org,2002:float

          In addition to arbitrary-precision floating-point numbers in scientific notation,
          YAML allows for three special values, which are not supported here:
          positive and negative infinity (.inf and -.inf), and "not a number (.nan)
          -}
          "float">:
            doc "Represents an approximation to real numbers"
            bigfloat,
          {-
          Represents arbitrary sized finite mathematical integers

          JSON schema: tag:yaml.org,2002:int
          -}
          "int">:
            doc "Represents arbitrary sized finite mathematical integers"
            bigint,
          {-
          Represents the lack of a value

          JSON schema: tag:yaml.org,2002:null
          -}
          "null">:
            doc "Represents the lack of a value"
            unit,
          {-
          Failsafe schema: tag:yaml.org,2002:str
          -}
          "str">:
            doc "A string value"
            string]]
