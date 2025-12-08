module Hydra.Ext.Sources.TypeScript.Model where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.typeScript.model"

define :: String -> Type -> Binding
define = defineType ns

ts :: String -> Type
ts = typeref ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "A basic TypeScript model, constructed on the basis of the typescriptlang.org documentation"
  where
    elements = [
      functionType,
      parameter,
      primitiveType_,
      type_]

functionType :: Binding
functionType = define "FunctionType" $
  T.record [
    "parameters">: T.list $ ts "Parameter",
    "range">: ts "Type"]

parameter :: Binding
parameter = define "Parameter" $
  T.record [
    "name">: T.string,
    "type">: ts "Type"]

primitiveType_ :: Binding
primitiveType_ = define "PrimitiveType" $
  T.union [
    "bigint">:
      doc "integers in the arbitrary precision format"
      T.unit,
    "boolean">:
      doc "true and false"
      T.unit,
    "null">:
      doc "equivalent to the unit type"
      T.unit,
    "number">:
      doc "a double-precision IEEE 754 floating point"
      T.unit,
    "object">:
      doc "similar to records"
      T.unit,
    "string">:
      doc "an immutable UTF-16 string"
      T.unit,
    "symbol">:
      doc "a unique value usually used as a key"
      T.unit,
    "undefined">:
      doc "also equivalent to the unit type"
      T.unit]

type_ :: Binding
type_ = define "Type" $
  T.union [
    "array">:
      doc "mutable arrays, also written Array<T>" $
      ts "Type",
    "function">:
      doc "functions" $
      ts "FunctionType",
    "never">:
      doc "the bottom type"
      T.unit,
    "objectLiteral">:
      doc "e.g. { property: Type }"
      T.unit,
    "primitive">:
      doc "A primitive type" $
      ts "PrimitiveType",
    "tuple">:
      doc "tuples, which are fixed-length but mutable" $
      T.list $ ts "Type",
    "unknown">:
      doc "The top type"
      T.unit,
    "void">:
      doc "for functions with no documented return value"
      T.unit]
