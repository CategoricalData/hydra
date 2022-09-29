module Hydra.Impl.Haskell.Sources.Ext.Json.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Module
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


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
