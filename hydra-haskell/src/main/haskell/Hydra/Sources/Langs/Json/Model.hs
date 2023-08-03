module Hydra.Sources.Langs.Json.Model where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


jsonModelModule :: Module Kv
jsonModelModule = Module ns elements [] $
    Just "A JSON syntax model. See the BNF at https://www.json.org"
  where
    ns = Namespace "hydra/langs/json/model"
    def = datatype ns
    json = typeref ns

    elements = [

      def "Value" $
        doc "A JSON value" $
        union [
          "array">: list $ json "Value",
          "boolean">: boolean,
          "null">: unit,
          "number">: bigfloat, -- TODO: JSON numbers are decimal-encoded
          "object">: Types.map string (json "Value"),
          "string">: string]]
