-- | Environment types for Haskell code generation.
-- These types support the Haskell coder and are used to track code generation state.

module Hydra.Sources.Haskell.Environment where

-- Standard type-level imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T


ns :: Namespace
ns = Namespace "hydra.haskell.environment"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [],
            moduleTypeDependencies = [],
            moduleDescription = Just "Environment types for Haskell code generation"}
  where
    definitions = [
      haskellModuleMetadata]

-- | Metadata for Haskell module generation.
haskellModuleMetadata :: Binding
haskellModuleMetadata = define "HaskellModuleMetadata" $
  doc "Metadata used to determine which standard imports are needed in a generated Haskell module" $
  T.record [
    "usesByteString">:
      doc "Whether the module uses Data.ByteString (B.ByteString)" $
      T.boolean,
    "usesInt">:
      doc "Whether the module uses Data.Int (I.Int8, I.Int16, I.Int64)" $
      T.boolean,
    "usesMap">:
      doc "Whether the module uses Data.Map (M.Map, M.fromList, M.empty)" $
      T.boolean,
    "usesSet">:
      doc "Whether the module uses Data.Set (S.Set, S.fromList, S.empty)" $
      T.boolean]
