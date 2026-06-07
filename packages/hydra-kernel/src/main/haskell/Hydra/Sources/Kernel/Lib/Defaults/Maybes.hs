
module Hydra.Sources.Kernel.Lib.Defaults.Maybes where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (maybe)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), maybe, map)
import qualified Data.List               as L

import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: ModuleName
ns = ModuleName "hydra.lib.defaults.maybes"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ExtractCore.ns, ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Default term-level implementations of Maybe functions for the Hydra interpreter."))}
  where
    definitions = [
      toDefinition cases_,
      toDefinition maybe_]

-- | Interpreter-friendly case analysis for Maybe terms (cases variant).
-- Takes optTerm, defaultTerm, funTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
cases_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
cases_ = define "cases" $
  doc "Interpreter-friendly case analysis for Maybe terms (cases argument order)." $
  "cx" ~> "g" ~> "optTerm" ~> "defaultTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly case analysis for Maybe terms.
-- Takes defaultTerm, funTerm, optTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
maybe_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
maybe_ = define "maybe" $
  doc "Interpreter-friendly case analysis for Maybe terms." $
  "cx" ~> "g" ~> "defaultTerm" ~> "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]
