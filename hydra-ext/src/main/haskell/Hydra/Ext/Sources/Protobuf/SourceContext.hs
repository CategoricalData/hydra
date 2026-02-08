module Hydra.Ext.Sources.Protobuf.SourceContext where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: Namespace
ns = Namespace "hydra.ext.protobuf.sourceContext"

define :: String -> Type -> Binding
define = defineType ns

pbSourceContext :: String -> Type
pbSourceContext = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just "Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/source_context.proto"
  where
    elements = [sourceContext]

sourceContext :: Binding
sourceContext = define "SourceContext" $
  doc ("`SourceContext` represents information about the source of a " ++
       "protobuf element, like the file in which it is defined.") $
  T.record [
    "fileName">:
      doc ("The path-qualified name of the .proto file that contained the associated " ++
           "protobuf element.  For example: `\"google/protobuf/source_context.proto\"`.")
      T.string]
