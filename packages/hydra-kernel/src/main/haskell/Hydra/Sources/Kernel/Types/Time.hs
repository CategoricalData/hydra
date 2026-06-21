module Hydra.Sources.Kernel.Types.Time where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.time"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "A model for points in time")}
  where
    definitions = [
      timespec]

timespec :: TypeDefinition
timespec = define "Timespec" $
  doc ("The POSIX struct timespec, with the same semantics: an instant in time as a number of"
    ++ " seconds and nanoseconds since the Unix Epoch (1970-01-01T00:00:00Z). The actual resolution"
    ++ " is implementation- and filesystem-defined. See <time.h>"
    ++ " (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/time.h.html) and XBD section"
    ++ " 4.19, \"Seconds Since the Epoch\""
    ++ " (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap04.html#tag_04_19).") $
  T.record [
    "seconds">:
      doc "Whole seconds since the Unix Epoch; signed, so instants in the far past or distant future are representable"
      T.int64,
    "nanoseconds">:
      doc "Nanoseconds within the second, in the range [0, 999999999]; unsigned, as the value is never negative"
      T.uint32]
