module Hydra.Ext.Sources.Protobuf.Any where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.protobuf.any"

define :: String -> Type -> Binding
define = defineType ns

pbAny :: String -> Type
pbAny = typeref ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/any.proto"
  where
    elements = [any_]

any_ :: Binding
any_ = define "Any" $
  doc ("`Any` contains an arbitrary serialized protocol buffer message along with a " ++
       "URL that describes the type of the serialized message.") $
  T.record [
    "typeUrl">:
      doc ("A URL/resource name that uniquely identifies the type of the serialized " ++
           "protocol buffer message.")
      T.string,
    "value">:
      doc "Must be a valid serialized protocol buffer of the above specified type."
      T.binary]
