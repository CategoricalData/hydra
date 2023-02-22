module Hydra.Sources.Ext.Json.Model where

import Hydra.Sources.Core

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard


jsonModelModule :: Module Meta
jsonModelModule = Module ns elements [] $
    Just "A JSON syntax model. See the BNF at https://www.json.org"
  where
    ns = Namespace "hydra/ext/json/model"
    def = datatype ns
    json = nsref ns

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
