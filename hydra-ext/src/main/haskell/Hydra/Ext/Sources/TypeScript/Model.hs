{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.TypeScript.Model where

-- Standard imports for type-level sources outside of the kernel
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
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


typeScriptNs = Namespace "hydra.ext.typeScript.model"
ts = typeref typeScriptNs

typeScriptModelModule :: Module
typeScriptModelModule = Module typeScriptNs elements [Core.module_] [Core.module_] $
    Just ("A basic TypeScript model, constructed on the basis of the typescriptlang.org documentation")
  where
    def = datatype typeScriptNs
    elements = [

      def "FunctionType" $
        record [
          "parameters">: list $ ts "Parameter",
          "range">: ts "Type"],

      def "Parameter" $
        record [
          "name">: string,
          "type">: ts "Type"],

      def "PrimitiveType" $
        union [
          "bigint">:
            doc "integers in the arbitrary precision format"
            unit,
          "boolean">:
            doc "true and false"
            unit,
          "null">:
            doc "equivalent to the unit type"
            unit,
          "number">:
            doc "a double-precision IEEE 754 floating point"
            unit,
          "object">:
            doc "similar to records"
            unit,
          "string">:
            doc "an immutable UTF-16 string"
            unit,
          "symbol">:
            doc "a unique value usually used as a key"
            unit,
          "undefined">:
            doc "also equivalent to the unit type"
            unit],

      def "Type" $
        union [
          "array">:
            doc "mutable arrays, also written Array<T>" $
            ts "Type",
          "function">:
            doc "functions" $
            ts "FunctionType",
          "never">:
            doc "the bottom type"
            unit,
          "objectLiteral">:
            doc "e.g. { property: Type }"
            unit,
          "primitive">:
            doc "A primitive type" $
            ts "PrimitiveType",
          "tuple">:
            doc "tuples, which are fixed-length but mutable" $
            list $ ts "Type",
          "unknown">:
            doc "The top type"
            unit,
          "void">:
            doc "for functions with no documented return value"
            unit]
        ]
