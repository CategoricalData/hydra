module Hydra.Sources.Kernel.Types.Validation where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.validation"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, ModuleName "hydra.error.core"],
            moduleMetadata = descriptionMetadata (Just "Framework types for configurable validation: profiles classify checks as errors or warnings, and results accumulate findings up to caller-specified bounds.")}
  where
    definitions = [
      validationProfile,
      validationResult]

validationProfile :: TypeDefinition
validationProfile = define "ValidationProfile" $
  doc "Configuration for a validation pass: which check rules are active, how each is classified, and the upper bounds on collected findings. A check whose rule name appears in neither set is never evaluated. Errors hard-stop the traversal once maxErrors is reached; warnings only stop being collected once maxWarnings is reached, never causing termination." $
  T.record [
    "errorRules">:
      doc "The set of fully qualified rule names whose findings are treated as errors. Each name has the form 'hydra.error.<package>.<UnionType>.<variant>', e.g. 'hydra.error.core.InvalidTermError.duplicateBinding'." $
      T.set Core.name,
    "warningRules">:
      doc "The set of fully qualified rule names whose findings are treated as warnings. Same name format as errorRules." $
      T.set Core.name,
    "maxErrors">:
      doc "Hard upper bound on collected errors. Validation terminates as soon as the errors list reaches this length. A value of 1 reproduces the legacy 'first error wins' behaviour." $
      T.int32,
    "maxWarnings">:
      doc "Soft upper bound on collected warnings. Once the warnings list reaches this length, further warning matches are silently dropped, but validation does not terminate; it continues until maxErrors is reached or the traversal completes." $
      T.int32]

validationResult :: TypeDefinition
validationResult = define "ValidationResult" $
  doc "The outcome of a validation pass: an ordered list of error findings and an ordered list of warning findings, each parameterized by the finding payload type. The pass is considered successful when 'errors' is empty; warnings are informational and do not affect that judgement." $
  T.forAll "e" $ T.record [
    "errors">:
      doc "Findings classified as errors, in traversal order, bounded by the profile's maxErrors." $
      T.list (T.var "e"),
    "warnings">:
      doc "Findings classified as warnings, in traversal order, bounded by the profile's maxWarnings." $
      T.list (T.var "e")]
