-- | All of Hydra's type-level kernel modules
module Hydra.Sources.Kernel.Types.All where

import Hydra.Kernel

import qualified Hydra.Sources.Kernel.Types.Paths        as Paths
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Docs        as Docs
import qualified Hydra.Sources.Kernel.Types.Error.Checking as ErrorChecking
import qualified Hydra.Sources.Kernel.Types.Error.Core     as ErrorCore
import qualified Hydra.Sources.Kernel.Types.Error.File     as ErrorFile
import qualified Hydra.Sources.Kernel.Types.Error.Packaging as ErrorPackaging
import qualified Hydra.Sources.Kernel.Types.Error.System   as ErrorSystem
import qualified Hydra.Sources.Kernel.Types.Errors     as Error
import qualified Hydra.Sources.Kernel.Types.File        as File
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Json.Model               as JsonModel
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
import qualified Hydra.Sources.Kernel.Types.Packaging   as Packaging
import qualified Hydra.Sources.Kernel.Types.Parsing     as Parsing
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Regex       as Regex
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.System      as System
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Time        as Time
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typed       as Typed
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Validation  as Validation


kernelTypesModules :: [Module]
kernelTypesModules = [
  Paths.module_,
  Ast.module_,
  Coders.module_,
  Core.module_,
  Docs.module_,
  ErrorChecking.module_,
  ErrorCore.module_,
  ErrorFile.module_,
  ErrorPackaging.module_,
  ErrorSystem.module_,
  Error.module_,
  File.module_,
  Graph.module_,
  JsonModel.module_,
  Packaging.module_,
  Parsing.module_,
  Query.module_,
  Regex.module_,
  Relational.module_,
  System.module_,
  Tabular.module_,
  Testing.module_,
  Time.module_,
  Topology.module_,
  Typed.module_,
  Typing.module_,
  Util.module_,
  Validation.module_,
  Variants.module_]

-- | Namespaces of all kernel type modules
kernelTypesModuleNames :: [ModuleName]
kernelTypesModuleNames = fmap moduleName kernelTypesModules
