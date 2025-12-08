{-# LANGUAGE OverloadedStrings #-}

-- | A simple JSON model. This model is part of the Hydra kernel, despite JSON being an external language; JSON support is built in to Hydra

module Hydra.Sources.Kernel.Types.Json where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.json"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A JSON syntax model. See the BNF at https://www.json.org"
  where
    elements = [
      value]

value :: Binding
value = define "Value" $
  doc "A JSON value" $
  T.union [
    "array">:
      doc "A JSON array" $
      T.list $ use value,
    "boolean">:
      doc "A boolean value" $
      T.boolean,
    "null">:
      doc "JSON's null value" $
      T.unit,
    "number">:
      doc "A numeric value" $
      T.bigfloat, -- TODO: JSON numbers are decimal-encoded
    "object">:
      doc "A JSON object as a set of key/value pairs" $
      T.map T.string (use value),
    "string">:
      doc "A string value" $
      T.string]
