-- | Per-target enumeration of the relative file paths a coder would
-- write for a given module. Mirrors the path-naming logic of each
-- target's 'moduleToX' coder function but without invoking the
-- (expensive) encoding pass.
--
-- Used by 'bootstrap-from-json --prune-stale' (#357) to compute the
-- keep-set from the package's owned modules, independent of which
-- modules Stage 7 freshness filtering ends up regenerating in this
-- run. The previous design called the coder for every module; that
-- forced bypassing Stage 7 and cost ~3m per regen of hydra-kernel.
module Hydra.TargetFilePaths (
  moduleFilePaths,
) where

import Hydra.Kernel
import qualified Hydra.Java.Coder as JavaCoder
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import qualified Data.List as L
import qualified Data.List.Split as LS

-- | Return the relative paths that 'target's coder would write for
-- 'm', given as paths under the source-set output dir
-- (<outBase>/<pkg>/src/<set>/<lang>/...). Empty list for an
-- unrecognized target, which makes prune a no-op for that target.
moduleFilePaths :: String -> Module -> [FilePath]
moduleFilePaths target m = case target of
  "haskell"     -> oneFilePerNamespace Util.CaseConventionPascal "hs"
  "python"      -> oneFilePerNamespace Util.CaseConventionLowerSnake "py"
  "scala"       -> oneFilePerNamespace Util.CaseConventionCamel "scala"
  "clojure"     -> oneFilePerNamespace Util.CaseConventionCamel "clj"
  "scheme"      -> oneFilePerNamespace Util.CaseConventionLowerSnake "scm"
  "common-lisp" -> oneFilePerNamespace Util.CaseConventionLowerSnake "lisp"
  "emacs-lisp"  -> oneFilePerNamespace Util.CaseConventionLowerSnake "el"
  "go"          -> [goNamespaceToFilePath ns]
  "java"        -> javaModulePaths m
  _             -> []
  where
    ns = moduleName m
    oneFilePerNamespace conv ext =
      [Names.moduleNameToFilePath conv (FileExtension ext) ns]

-- | Mirror of Hydra.Go.Coder.goNamespaceToFilePath (not exported there).
goNamespaceToFilePath :: ModuleName -> FilePath
goNamespaceToFilePath (ModuleName ns) =
  let parts = LS.splitOn "." ns
      dirPath = L.intercalate "/" parts
      fileName = case reverse parts of
        (p:_) -> p
        []    -> "main"
  in dirPath ++ "/" ++ fileName ++ ".go"

-- | Java emits one file per top-level type (filtered by isNominalType)
-- plus an Elements interface if the module contains any term defs.
-- Path computation mirrors 'bindingNameToFilePath' in
-- 'Hydra.Java.Coder'.
javaModulePaths :: Module -> [FilePath]
javaModulePaths m =
  let typeDefs = [td | DefinitionType td <- moduleDefinitions m]
      termDefs = [td | DefinitionTerm td <- moduleDefinitions m]
      keepType td =
        let typ = typeSchemeBody (typeDefinitionTypeScheme td)
        in Predicates.isNominalType typ
      typeFiles =
        [ JavaCoder.bindingNameToFilePath (typeDefinitionName td)
        | td <- typeDefs, keepType td ]
      elementsFile =
        if null termDefs
          then []
          else [ JavaCoder.bindingNameToFilePath
                   (JavaCoder.elementsQualifiedName (moduleName m)) ]
  in typeFiles ++ elementsFile
