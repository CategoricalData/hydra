module Hydra.Sources.Build.CompareReport where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


-- | Type-level model for the JSON snapshot compare-report (#416 P4): the pure
-- per-module comparison record + the whole-report summary that the three native
-- generator drivers (generate-hydra-{java,python,scala}-from-*.sh) previously
-- each hand-rolled as an inline-Python heredoc. Everything here is a data model;
-- the comparison logic lives in the sibling terms module
-- (Hydra.Sources.Build.CompareReportLogic, hydra.build.comparereportlogic), and
-- the file reads use hydra.lib.files.readFile/listDirectory. See #416.
ns :: ModuleName
ns = ModuleName "hydra.build.comparereport"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata (Just ("Type-level model for the JSON snapshot compare-report:"
              ++ " the per-module comparison record and the whole-report summary shared by the native"
              ++ " generator drivers. See https://github.com/CategoricalData/hydra/issues/416"))}
  where
    definitions = [
      compareReport,
      compareStatus,
      moduleCompare]

-- | The whole compare-report: every module's record plus the byte-identical
-- tally used for the pass/fail verdict and the summary line.
compareReport :: TypeDefinition
compareReport = define "CompareReport" $
  doc "The whole compare-report: per-module records plus the byte-identical tally" $
  T.record [
    "rows">:
      doc "One record per compared module"
      (T.list moduleCompare),
    "byteEqCount">:
      doc "How many modules were byte-identical"
      T.int32,
    "total">:
      doc "How many modules were compared"
      T.int32]

-- | The result of comparing one module's generated JSON on the two sides
-- (freshly generated "ours" vs the committed canonical dist/json).
compareStatus :: TypeDefinition
compareStatus = define "CompareStatus" $
  doc "The result of comparing one module's JSON on the two sides" $
  T.union [
    "byteEq">:
      doc "The two sides are byte-identical"
      T.unit,
    "differ">:
      doc "Both sides exist but differ"
      T.unit,
    "missing">:
      doc "Our side is absent (the canonical side has the module, ours does not)"
      T.unit]

-- | One module's comparison record: its name, the comparison status, and the two
-- byte sizes. The size delta (ours - canon) is derived, not stored.
moduleCompare :: TypeDefinition
moduleCompare = define "ModuleCompare" $
  doc "One module's comparison record" $
  T.record [
    "module">:
      doc "The module name (the JSON file's base name, without the .json suffix)"
      T.string,
    "status">:
      doc "The comparison status"
      compareStatus,
    "ourSize">:
      doc "The byte size of our (freshly generated) side, or 0 if missing"
      T.int32,
    "canonSize">:
      doc "The byte size of the canonical side"
      T.int32,
    "diffLines">:
      doc "The number of differing lines (0 when byte-identical)"
      T.int32]
