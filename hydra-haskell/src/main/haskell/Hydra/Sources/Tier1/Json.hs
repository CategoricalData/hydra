-- | A simple JSON model. This model is part of the Hydra kernel, despite JSON being an external language; JSON support is built in to Hydra

module Hydra.Sources.Tier1.Json where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y


jsonModelModule :: Module
jsonModelModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "A JSON syntax model. See the BNF at https://www.json.org"
  where
    ns = Namespace "hydra/json"
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
