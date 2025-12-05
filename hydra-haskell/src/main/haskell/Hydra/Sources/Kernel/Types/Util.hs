{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Util where

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
    Just ("General-purpose utility types used across Hydra.")
  where
    ns = Namespace "hydra.util"
    util = typeref ns
    def = datatype ns

    elements = [

      def "CaseConvention" $
        doc "A naming convention for symbols, such as camelCase or snake_case" $
        Types.enum ["camel", "pascal", "lowerSnake", "upperSnake"],

      def "Comparison" $
        doc "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      def "Precision" $
        doc "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        union [
          "arbitrary">:
            doc "Arbitrary precision" $
            unit,
          "bits">:
            doc "Precision to a specified number of bits" $
            int32]]
