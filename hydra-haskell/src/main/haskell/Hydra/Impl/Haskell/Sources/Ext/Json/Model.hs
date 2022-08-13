module Hydra.Impl.Haskell.Sources.Ext.Json.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


jsonModelModule :: Module Meta
jsonModelModule = Module jsonModel []

jsonModelName :: GraphName
jsonModelName = GraphName "hydra/ext/json/model"

-- | See the BNF at https://www.json.org
jsonModel :: Graph Meta
jsonModel = Graph jsonModelName elements hydraCoreName
  where
    def = datatype coreContext jsonModelName
    json = nsref jsonModelName

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
