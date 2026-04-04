-- | The CppEnvironment type used by C++ code generation.

module Hydra.Ext.Sources.Cpp.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Packaging as ModuleTypes
import qualified Hydra.Ext.Sources.Cpp.Syntax as CppSyntax

ns :: Namespace
ns = Namespace "hydra.ext.cpp.environment"

define :: String -> Type -> Binding
define = defineType ns

coreType :: String -> Type
coreType = typeref CoreTypes.ns

modulType :: String -> Type
modulType = typeref ModuleTypes.ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [CoreTypes.ns, ModuleTypes.ns] $
    Just "Type definitions for C++ code generation environment"
  where
    definitions = [
      cppEnvironmentType]

-- | The CppEnvironment type definition
cppEnvironmentType :: Binding
cppEnvironmentType = define "CppEnvironment" $
  doc "Environment for C++ code generation" $
  T.record [
    "namespaces" >:
      doc "Namespace mapping for code generation" $
      T.apply (modulType "Namespaces") T.string,
    "boundTypeVariables" >:
      doc "Type variables in scope, with their C++ names" $
      T.pair (T.list (coreType "Name")) (T.map (coreType "Name") T.string)]
