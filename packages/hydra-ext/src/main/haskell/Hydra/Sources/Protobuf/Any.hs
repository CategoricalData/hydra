module Hydra.Sources.Protobuf.Any where

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


ns :: ModuleName
ns = ModuleName "hydra.protobuf.any"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/any.proto")}
  where
    definitions = [any_]

any_ :: TypeDefinition
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

pbAny :: String -> Type
pbAny = typeref ns
