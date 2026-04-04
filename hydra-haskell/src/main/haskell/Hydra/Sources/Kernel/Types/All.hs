-- | All of Hydra's type-level kernel modules
module Hydra.Sources.Kernel.Types.All where

import Hydra.Kernel

import qualified Hydra.Sources.Kernel.Types.Paths        as Paths
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Classes     as Classes
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Context     as Context
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Error.Checking as ErrorChecking
import qualified Hydra.Sources.Kernel.Types.Error.Core     as ErrorCore
import qualified Hydra.Sources.Kernel.Types.Error.Packaging as ErrorPackaging
import qualified Hydra.Sources.Kernel.Types.Errors      as Errors
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Json.Model               as JsonModel
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
import qualified Hydra.Sources.Kernel.Types.Packaging   as Packaging
import qualified Hydra.Sources.Kernel.Types.Parsing     as Parsing
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util


kernelTypesModules :: [Module]
kernelTypesModules = [
  Paths.module_,
  Ast.module_,
  Classes.module_,
  Coders.module_,
  Context.module_,
  Core.module_,
  ErrorChecking.module_,
  ErrorCore.module_,
  ErrorPackaging.module_,
  Errors.module_,
  Graph.module_,
  JsonModel.module_,
  Packaging.module_,
  Parsing.module_,
  Phantoms.module_,
  Query.module_,
  Relational.module_,
  Tabular.module_,
  Testing.module_,
  Topology.module_,
  Typing.module_,
  Util.module_,
  Variants.module_]

-- | Namespaces of all kernel type modules
kernelTypesNamespaces :: [Namespace]
kernelTypesNamespaces = fmap moduleNamespace kernelTypesModules
