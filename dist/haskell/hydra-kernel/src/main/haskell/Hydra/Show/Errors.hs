-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.error types

module Hydra.Show.Errors where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Error.Core as ShowErrorCore
import qualified Hydra.Show.Typing as ShowTyping
import qualified Hydra.Show.Variants as ShowVariants
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a checking error as a string
checkingError :: Checking.CheckingError -> String
checkingError ce =
    case ce of
      Checking.CheckingErrorIncorrectUnification v0 -> incorrectUnificationError v0
      Checking.CheckingErrorNotAForallType v0 -> notAForallTypeError v0
      Checking.CheckingErrorNotAFunctionType v0 -> notAFunctionTypeError v0
      Checking.CheckingErrorTypeArityMismatch v0 -> typeArityMismatchError v0
      Checking.CheckingErrorTypeMismatch v0 -> typeMismatchError v0
      Checking.CheckingErrorUnboundTypeVariables v0 -> unboundTypeVariablesError v0
      Checking.CheckingErrorUnequalTypes v0 -> unequalTypesError v0
      Checking.CheckingErrorUnsupportedTermVariant v0 -> unsupportedTermVariantError v0
      Checking.CheckingErrorUntypedLambda v0 -> untypedLambdaError v0
      Checking.CheckingErrorUntypedLetBinding v0 -> untypedLetBindingError v0
-- | Show a decoding error as a string
decodingError :: Errors.DecodingError -> String
decodingError de = Strings.cat2 "decoding error: " (Errors.unDecodingError de)
-- | Show an error as a string
error :: Errors.Error -> String
error e =
    case e of
      Errors.ErrorChecking v0 -> checkingError v0
      Errors.ErrorDecoding v0 -> decodingError v0
      Errors.ErrorDuplicateBinding v0 -> ShowErrorCore.duplicateBindingError v0
      Errors.ErrorDuplicateField v0 -> ShowErrorCore.duplicateFieldError v0
      Errors.ErrorExtraction _ -> "extraction error"
      Errors.ErrorInference _ -> "inference error"
      Errors.ErrorOther v0 -> otherError v0
      Errors.ErrorResolution v0 -> resolutionError v0
      Errors.ErrorUndefinedField v0 -> ShowErrorCore.undefinedFieldError v0
      Errors.ErrorUndefinedTermVariable v0 -> ShowErrorCore.undefinedTermVariableError v0
      Errors.ErrorUntypedTermVariable v0 -> ShowErrorCore.untypedTermVariableError v0
      Errors.ErrorUnexpectedTermVariant v0 -> ShowErrorCore.unexpectedTermVariantError v0
      Errors.ErrorUnexpectedTypeVariant v0 -> ShowErrorCore.unexpectedTypeVariantError v0
      Errors.ErrorUnification v0 -> unificationError v0
-- | Show an incorrect unification error as a string
incorrectUnificationError :: Checking.IncorrectUnificationError -> String
incorrectUnificationError e =

      let subst = Checking.incorrectUnificationErrorSubstitution e
      in (Strings.cat2 "incorrect unification: " (ShowTyping.typeSubst subst))
-- | Show a not-a-forall-type error as a string
notAForallTypeError :: Checking.NotAForallTypeError -> String
notAForallTypeError e =

      let typ = Checking.notAForallTypeErrorType e
          args = Checking.notAForallTypeErrorTypeArguments e
      in (Strings.cat [
        "not a forall type: ",
        (ShowCore.type_ typ),
        ". Trying to apply ",
        (Literals.showInt32 (Lists.length args)),
        " type argument(s): ",
        (Formatting.showList ShowCore.type_ args)])
-- | Show a not-a-function-type error as a string
notAFunctionTypeError :: Checking.NotAFunctionTypeError -> String
notAFunctionTypeError e =

      let typ = Checking.notAFunctionTypeErrorType e
      in (Strings.cat2 "not a function type: " (ShowCore.type_ typ))
-- | Show an other error as a string
otherError :: Errors.OtherError -> String
otherError oe = Errors.unOtherError oe
-- | Show a resolution error as a string, including the offending name or shape
resolutionError :: Errors.ResolutionError -> String
resolutionError re =
    case re of
      Errors.ResolutionErrorNoSuchBinding v0 -> Strings.cat2 "no such binding: " (Core.unName (Errors.noSuchBindingErrorName v0))
      Errors.ResolutionErrorNoSuchPrimitive v0 -> Strings.cat2 "no such primitive: " (Core.unName (Errors.noSuchPrimitiveErrorName v0))
      Errors.ResolutionErrorNoMatchingField v0 -> Strings.cat2 "no matching field: " (Core.unName (Errors.noMatchingFieldErrorFieldName v0))
      Errors.ResolutionErrorOther v0 -> Strings.cat2 "resolution error: " (Errors.unOtherResolutionError v0)
      Errors.ResolutionErrorUnexpectedShape v0 -> Strings.cat [
        "unexpected shape: expected ",
        (Errors.unexpectedShapeErrorExpected v0),
        " but got ",
        (Errors.unexpectedShapeErrorActual v0)]
-- | Show a type arity mismatch error as a string
typeArityMismatchError :: Checking.TypeArityMismatchError -> String
typeArityMismatchError e =

      let typ = Checking.typeArityMismatchErrorType e
          expected = Checking.typeArityMismatchErrorExpectedArity e
          actual = Checking.typeArityMismatchErrorActualArity e
          args = Checking.typeArityMismatchErrorTypeArguments e
      in (Strings.cat [
        "type ",
        (ShowCore.type_ typ),
        " applied to the wrong number of type arguments (expected ",
        (Literals.showInt32 expected),
        ", got ",
        (Literals.showInt32 actual),
        "): ",
        (Formatting.showList ShowCore.type_ args)])
-- | Show a type mismatch error as a string
typeMismatchError :: Checking.TypeMismatchError -> String
typeMismatchError e =

      let expected = Checking.typeMismatchErrorExpectedType e
          actual = Checking.typeMismatchErrorActualType e
      in (Strings.cat [
        "type mismatch: expected ",
        (ShowCore.type_ expected),
        " but found ",
        (ShowCore.type_ actual)])
-- | Show an unbound type variables error as a string
unboundTypeVariablesError :: Checking.UnboundTypeVariablesError -> String
unboundTypeVariablesError e =

      let vars = Checking.unboundTypeVariablesErrorVariables e
          typ = Checking.unboundTypeVariablesErrorType e
      in (Strings.cat [
        "unbound type variables: {",
        (Strings.intercalate ", " (Lists.map Core.unName (Sets.toList vars))),
        "} in type ",
        (ShowCore.type_ typ)])
-- | Show an unequal types error as a string
unequalTypesError :: Checking.UnequalTypesError -> String
unequalTypesError e =

      let types = Checking.unequalTypesErrorTypes e
          desc = Checking.unequalTypesErrorDescription e
      in (Strings.cat [
        "unequal types ",
        (Formatting.showList ShowCore.type_ types),
        " in ",
        desc])
-- | Show a unification error as a string
unificationError :: Errors.UnificationError -> String
unificationError e =

      let lt = Errors.unificationErrorLeftType e
          rt = Errors.unificationErrorRightType e
          msg = Errors.unificationErrorMessage e
      in (Strings.cat [
        "unification error: cannot unify ",
        (ShowCore.type_ lt),
        " with ",
        (ShowCore.type_ rt),
        ": ",
        msg])
-- | Show an unsupported term variant error as a string
unsupportedTermVariantError :: Checking.UnsupportedTermVariantError -> String
unsupportedTermVariantError e =
    Strings.cat2 "unsupported term variant: " (ShowVariants.termVariant (Checking.unsupportedTermVariantErrorTermVariant e))
-- | Show an untyped lambda error as a string
untypedLambdaError :: t0 -> String
untypedLambdaError _ = "untyped lambda"
-- | Show an untyped let binding error as a string
untypedLetBindingError :: Checking.UntypedLetBindingError -> String
untypedLetBindingError e =

      let b = Checking.untypedLetBindingErrorBinding e
      in (Strings.cat2 "untyped let binding: " (ShowCore.binding b))
