-- | Derives a module's declared dependencies from the modules its definitions actually reference (#837).
--
-- A module's canonical dependency list is the set of modules it references, which is exactly what the
-- textual syntax's printer emits as import lines (see Print.Packaging.moduleAliases). Hand-maintained
-- lists in the DSL sources tended to over-declare (e.g. depending on every types module), so the DSL and
-- .hydra forms disagreed. Applying 'alignDependencies' where the kernel module lists are assembled makes
-- the DSL form agree with the .hydra form by construction.
module Hydra.Sources.Kernel.AlignDependencies (alignDependencies) where

import Hydra.Kernel
import qualified Hydra.Core.Analysis as Analysis
import qualified Hydra.Core.Overlay.Haskell.Bootstrap as Bootstrap

import qualified Data.Set as S

-- | Replace a module's dependencies with the (sorted) set of other modules its definitions reference.
-- The reference rule (including the primitive-shape modules a primitive implicitly needs) lives in
-- 'Analysis.definitionDependencyModuleNames', shared with the textual-syntax printer.
alignDependencies :: Module -> Module
alignDependencies m = m {
  moduleDependencies = Bootstrap.unqualifiedDep <$> S.toAscList referenced }
  where
    referenced = S.delete (moduleName m) (Analysis.definitionDependencyModuleNames (moduleDefinitions m))
