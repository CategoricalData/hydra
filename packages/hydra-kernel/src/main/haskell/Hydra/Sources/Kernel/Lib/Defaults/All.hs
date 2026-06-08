
module Hydra.Sources.Kernel.Lib.Defaults.All where

import Hydra.Kernel


-- | All default library modules. As of issue #437 every former default has been
-- merged into its canonical Lib/<Sub>.hs module via toPrimitive (or, for
-- irreducible eliminators, intentionally left as primNoDef with no portable
-- term-level default). The list is retained as an empty placeholder so that
-- bootstrap-from-json/update-json-* and the manifest writer keep their
-- defaultLibModules field present (and empty) until those consumers are
-- updated to drop the concept entirely.
defaultLibModules :: [Module]
defaultLibModules = []

-- | Module names of all default library modules.
defaultLibModuleNames :: [ModuleName]
defaultLibModuleNames = Prelude.map moduleName defaultLibModules
