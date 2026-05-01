
module Hydra.Sources.Kernel.Lib.Default.All where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths     as Paths
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Util       as Util
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Json.Model          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Packaging        as Packaging
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Meta.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Lib.Default.Eithers as DefaultEithers
import qualified Hydra.Sources.Kernel.Lib.Default.Equality as DefaultEquality
import qualified Hydra.Sources.Kernel.Lib.Default.Lists as DefaultLists
import qualified Hydra.Sources.Kernel.Lib.Default.Logic as DefaultLogic
import qualified Hydra.Sources.Kernel.Lib.Default.Maps as DefaultMaps
import qualified Hydra.Sources.Kernel.Lib.Default.Math as DefaultMath
import qualified Hydra.Sources.Kernel.Lib.Default.Maybes as DefaultMaybes
import qualified Hydra.Sources.Kernel.Lib.Default.Pairs as DefaultPairs
import qualified Hydra.Sources.Kernel.Lib.Default.Sets as DefaultSets


ns :: Namespace
ns = Namespace "hydra.lib.default"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

-- | All default library modules (term-level fallback implementations used by the interpreter
-- when a host does not supply a native primitive).
-- Note: Eithers, Lists, Maps, Maybes, and Sets modules cannot currently be code-generated
-- due to DSL type inference limitations. The interpreter-level implementations in those modules
-- use meta-level DSL functions (Maybes.maybe, Eithers.either_, etc.) applied to Term-level
-- values, which causes unification errors. These modules still work at runtime via the
-- native Haskell implementations registered via DefaultPrimitives.
defaultLibModules :: [Module]
defaultLibModules = [
  DefaultEithers.module_,
  DefaultEquality.module_,
  DefaultLists.module_,
  DefaultLogic.module_,
  DefaultMaps.module_,
  DefaultMath.module_,
  DefaultMaybes.module_,
  DefaultPairs.module_,
  DefaultSets.module_
  ]

-- | Namespaces of all default library modules
defaultLibNamespaces :: [Namespace]
defaultLibNamespaces = Prelude.map moduleNamespace defaultLibModules

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = defaultLibNamespaces,
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = Just ("Registry of default term-level library implementations used by the Hydra interpreter when no native primitive is available.")}
  where
    definitions = []
