module Hydra.Sources.Yaml.Model where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Meta        as Meta
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow


yamlModelModule :: Module
yamlModelModule = Module ns elements [Core.module_] [Core.module_] $
    Just ("A basic YAML representation model. Based on:\n" ++
      "  https://yaml.org/spec/1.2/spec.html\n" ++
      "The Serialization and Presentation properties of YAML,\n" ++
      "including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.\n" ++
      "In addition, tags are omitted from this model, and non-standard scalars are unsupported.")
  where
    ns = Namespace "hydra.ext.org.yaml.model"
    def = datatype ns
    model = typeref ns

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
