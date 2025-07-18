-- | A simple JSON model. This model is part of the Hydra kernel, despite JSON being an external language; JSON support is built in to Hydra

module Hydra.Sources.Kernel.Types.Json where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A JSON syntax model. See the BNF at https://www.json.org"
  where
    ns = Namespace "hydra.json"
    def = datatype ns
    json = typeref ns

    elements = [

      def "Value" $
        doc "A JSON value" $
        union [
          "array">:
            doc "A JSON array" $
            list $ json "Value",
          "boolean">:
            doc "A boolean value"
            boolean,
          "null">:
            doc "JSON's null value"
            unit,
          "number">:
            doc "A numeric value"
            bigfloat, -- TODO: JSON numbers are decimal-encoded
          "object">:
            doc "A JSON object as a set of key/value pairs" $
            Types.map string (json "Value"),
          "string">:
            doc "A string value"
            string]]
