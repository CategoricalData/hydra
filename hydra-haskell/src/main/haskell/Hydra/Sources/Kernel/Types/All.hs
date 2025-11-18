-- | All of Hydra's type-level kernel modules
module Hydra.Sources.Kernel.Types.All where

import Hydra.Kernel

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Classes     as Classes
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
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


kernelTypesModules :: [Module]
kernelTypesModules = [
  Accessors.module_,
  Ast.module_,
  Classes.module_,
  Coders.module_,
  Compute.module_,
  Constraints.module_,
  Core.module_,
  Grammar.module_,
  Graph.module_,
  Json.module_,
  Module.module_,
  Phantoms.module_,
  Query.module_,
  Relational.module_,
  Tabular.module_,
  Testing.module_,
  Topology.module_,
  Typing.module_,
  Util.module_,
  Variants.module_,
  Workflow.module_]
