-- | The CppEnvironment type used by C++ code generation.

module Hydra.Sources.Cpp.Environment where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.Sources.Kernel.Types.Packaging as ModuleTypes
import qualified Hydra.Sources.Kernel.Types.Util as UtilTypes
import qualified Hydra.Sources.Cpp.Syntax as CppSyntax

ns :: ModuleName
ns = ModuleName "hydra.cpp.environment"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [CoreTypes.ns, ModuleTypes.ns, UtilTypes.ns],
            moduleDescription = Just "Type definitions for C++ code generation environment"}
  where
    definitions = [
      cppEnvironmentType]

coreType :: String -> Type
coreType = typeref CoreTypes.ns

-- | The CppEnvironment type definition
cppEnvironmentType :: TypeDefinition
cppEnvironmentType = define "CppEnvironment" $
  doc "Environment for C++ code generation" $
  T.record [
    "namespaces" >:
      doc "ModuleName mapping for code generation" $
      T.apply (utilType "ModuleNames") T.string,
    "boundTypeVariables" >:
      doc "Type variables in scope, with their C++ names" $
      T.pair (T.list (coreType "Name")) (T.map (coreType "Name") T.string)]

modulType :: String -> Type
modulType = typeref ModuleTypes.ns

utilType :: String -> Type
utilType = typeref UtilTypes.ns
