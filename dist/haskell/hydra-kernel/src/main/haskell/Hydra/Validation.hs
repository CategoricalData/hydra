-- Note: this is an automatically generated file. Do not edit.
-- | Framework types for configurable validation: profiles classify checks as errors or warnings, and results accumulate findings up to caller-specified bounds.

module Hydra.Validation where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Configuration for a validation pass: which check rules are active, how each is classified, and the upper bounds on collected findings. A check whose rule name appears in neither set is never evaluated. Errors hard-stop the traversal once maxErrors is reached; warnings only stop being collected once maxWarnings is reached, never causing termination.
data ValidationProfile =
  ValidationProfile {
    -- | The set of fully qualified rule names whose findings are treated as errors. Each name has the form 'hydra.error.<package>.<UnionType>.<variant>', e.g. 'hydra.error.core.InvalidTermError.duplicateBinding'.
    validationProfileErrorRules :: (S.Set Core.Name),
    -- | The set of fully qualified rule names whose findings are treated as warnings. Same name format as errorRules.
    validationProfileWarningRules :: (S.Set Core.Name),
    -- | Hard upper bound on collected errors. Validation terminates as soon as the errors list reaches this length. A value of 1 reproduces the legacy 'first error wins' behaviour.
    validationProfileMaxErrors :: Int,
    -- | Soft upper bound on collected warnings. Once the warnings list reaches this length, further warning matches are silently dropped, but validation does not terminate; it continues until maxErrors is reached or the traversal completes.
    validationProfileMaxWarnings :: Int}
  deriving (Eq, Ord, Read, Show)
_ValidationProfile = Core.Name "hydra.validation.ValidationProfile"
_ValidationProfile_errorRules = Core.Name "errorRules"
_ValidationProfile_warningRules = Core.Name "warningRules"
_ValidationProfile_maxErrors = Core.Name "maxErrors"
_ValidationProfile_maxWarnings = Core.Name "maxWarnings"
-- | The outcome of a validation pass: an ordered list of error findings and an ordered list of warning findings, each parameterized by the finding payload type. The pass is considered successful when 'errors' is empty; warnings are informational and do not affect that judgement.
data ValidationResult e =
  ValidationResult {
    -- | Findings classified as errors, in traversal order, bounded by the profile's maxErrors.
    validationResultErrors :: [e],
    -- | Findings classified as warnings, in traversal order, bounded by the profile's maxWarnings.
    validationResultWarnings :: [e]}
  deriving (Eq, Ord, Read, Show)
_ValidationResult = Core.Name "hydra.validation.ValidationResult"
_ValidationResult_errors = Core.Name "errors"
_ValidationResult_warnings = Core.Name "warnings"
