
module Hydra.Sources.Kernel.Terms.Print.Errors where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Error.Core as PrintErrorCore
import qualified Hydra.Sources.Kernel.Terms.Print.Variants as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting


ns :: ModuleName
ns = ModuleName "hydra.core.print.errors"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([PrintCore.ns, PrintErrorCore.ns, PrintVariants.ns, PrintTyping.ns, Formatting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.error types")}
  where
   definitions = [
     toDefinition checkingError,
     toDefinition decodingError,
     toDefinition error_,
     toDefinition extractionError,
     toDefinition incorrectUnificationError,
     toDefinition inferenceError,
     toDefinition notAForallTypeError,
     toDefinition notAFunctionTypeError,
     toDefinition otherError,
     toDefinition resolutionError,
     toDefinition subtermPath,
     toDefinition subtermStep,
     toDefinition typeArityMismatchError,
     toDefinition typeMismatchError,
     toDefinition unboundTypeVariablesError,
     toDefinition unequalTypesError,
     toDefinition unificationError,
     toDefinition unsupportedTermVariantError,
     toDefinition untypedLambdaError,
     toDefinition untypedLetBindingError]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

checkingError :: TypedTermDefinition (CheckingError -> String)
checkingError = define "checkingError" $
  doc "Show a checking error as a string" $
  "ce" ~> match _CheckingError (var "ce") Nothing [
    _CheckingError_incorrectUnification>>: incorrectUnificationError,
    _CheckingError_notAForallType>>: notAForallTypeError,
    _CheckingError_notAFunctionType>>: notAFunctionTypeError,
    _CheckingError_typeArityMismatch>>: typeArityMismatchError,
    _CheckingError_typeMismatch>>: typeMismatchError,
    _CheckingError_unboundTypeVariables>>: unboundTypeVariablesError,
    _CheckingError_unequalTypes>>: unequalTypesError,
    _CheckingError_unsupportedTermVariant>>: unsupportedTermVariantError,
    _CheckingError_untypedLambda>>: untypedLambdaError,
    _CheckingError_untypedLetBinding>>: untypedLetBindingError]

decodingError :: TypedTermDefinition (DecodingError -> String)
decodingError = define "decodingError" $
  doc "Show a decoding error as a string" $
  "de" ~> Strings.concat2 (string "decoding error: ") (unwrap _DecodingError @@ var "de")

error_ :: TypedTermDefinition (Error -> String)
error_ = define "error" $
  doc "Show an error as a string" $
  "e" ~> match _Error (var "e") Nothing [
    _Error_checking>>: checkingError,
    _Error_decoding>>: decodingError,
    _Error_duplicateBinding>>: PrintErrorCore.duplicateBindingError,
    _Error_duplicateField>>: PrintErrorCore.duplicateFieldError,
    _Error_extraction>>: extractionError,
    _Error_inference>>: inferenceError,
    _Error_other>>: otherError,
    _Error_resolution>>: resolutionError,
    _Error_undefinedField>>: PrintErrorCore.undefinedFieldError,
    _Error_undefinedTermVariable>>: PrintErrorCore.undefinedTermVariableError,
    _Error_untypedTermVariable>>: PrintErrorCore.untypedTermVariableError,
    _Error_unexpectedTermVariant>>: PrintErrorCore.unexpectedTermVariantError,
    _Error_unexpectedTypeVariant>>: PrintErrorCore.unexpectedTypeVariantError,
    _Error_unification>>: unificationError]

extractionError :: TypedTermDefinition (ExtractionError -> String)
extractionError = define "extractionError" $
  doc "Show an extraction error as a string" $
  "e" ~> match _ExtractionError (var "e") Nothing [
    _ExtractionError_emptyList>>: "_" ~>
      string "empty list",
    _ExtractionError_multipleBindings>>: "e" ~>
      Strings.concat2 (string "multiple bindings with name: ")
        (Core.unName $ project _MultipleBindingsError _MultipleBindingsError_name @@ var "e"),
    _ExtractionError_multipleFields>>: "e" ~>
      Strings.concat2 (string "multiple fields with name: ")
        (Core.unName $ project _MultipleFieldsError _MultipleFieldsError_fieldName @@ var "e"),
    _ExtractionError_noMatchingField>>: "e" ~>
      Strings.concat2 (string "no matching field: ")
        (Core.unName $ project _NoMatchingFieldError _NoMatchingFieldError_fieldName @@ var "e"),
    _ExtractionError_noSuchBinding>>: "e" ~>
      Strings.concat2 (string "no such binding: ")
        (Core.unName $ project _NoSuchBindingError _NoSuchBindingError_name @@ var "e"),
    _ExtractionError_notEnoughCases>>: "_" ~>
      string "not enough cases",
    _ExtractionError_unexpectedShape>>: "e" ~>
      Strings.concat $ list [
        string "unexpected shape: expected ",
        project _UnexpectedShapeError _UnexpectedShapeError_expected @@ var "e",
        string " but got ",
        project _UnexpectedShapeError _UnexpectedShapeError_actual @@ var "e"]]

incorrectUnificationError :: TypedTermDefinition (IncorrectUnificationError -> String)
incorrectUnificationError = define "incorrectUnificationError" $
  doc "Show an incorrect unification error as a string" $
  "e" ~>
  "subst" <~ project _IncorrectUnificationError _IncorrectUnificationError_substitution @@ var "e" $
  Strings.concat2 (string "incorrect unification: ") (PrintTyping.typeSubst @@ var "subst")

inferenceError :: TypedTermDefinition (InferenceError -> String)
inferenceError = define "inferenceError" $
  doc "Show an inference error as a string, including the subterm path where applicable" $
  "e" ~> match _InferenceError (var "e") Nothing [
    _InferenceError_checking>>: checkingError,
    _InferenceError_other>>: "e" ~>
      "path" <~ project _OtherInferenceError _OtherInferenceError_path @@ var "e" $
      "msg" <~ project _OtherInferenceError _OtherInferenceError_message @@ var "e" $
      Strings.concat $ list [
        string "inference error at ",
        subtermPath @@ var "path",
        string ": ",
        var "msg"],
    _InferenceError_unification>>: "e" ~>
      "path" <~ project _UnificationInferenceError _UnificationInferenceError_path @@ var "e" $
      "cause" <~ project _UnificationInferenceError _UnificationInferenceError_cause @@ var "e" $
      Strings.concat $ list [
        string "inference error at ",
        subtermPath @@ var "path",
        string ": ",
        unificationError @@ var "cause"]]

notAForallTypeError :: TypedTermDefinition (NotAForallTypeError -> String)
notAForallTypeError = define "notAForallTypeError" $
  doc "Show a not-a-forall-type error as a string" $
  "e" ~>
  "typ" <~ project _NotAForallTypeError _NotAForallTypeError_type @@ var "e" $
  "args" <~ project _NotAForallTypeError _NotAForallTypeError_typeArguments @@ var "e" $
  Strings.concat $ list [
    string "not a forall type: ",
    PrintCore.type_ @@ var "typ",
    string ". Trying to apply ",
    Literals.printInt32 (Lists.length $ var "args"),
    string " type argument(s): ",
    Formatting.showList @@ PrintCore.type_ @@ var "args"]

notAFunctionTypeError :: TypedTermDefinition (NotAFunctionTypeError -> String)
notAFunctionTypeError = define "notAFunctionTypeError" $
  doc "Show a not-a-function-type error as a string" $
  "e" ~>
  "typ" <~ project _NotAFunctionTypeError _NotAFunctionTypeError_type @@ var "e" $
  Strings.concat2 (string "not a function type: ") (PrintCore.type_ @@ var "typ")

otherError :: TypedTermDefinition (OtherError -> String)
otherError = define "otherError" $
  doc "Show an other error as a string" $
  "oe" ~> unwrap _OtherError @@ var "oe"

resolutionError :: TypedTermDefinition (ResolutionError -> String)
resolutionError = define "resolutionError" $
  doc "Show a resolution error as a string, including the offending name or shape" $
  "re" ~> match _ResolutionError (var "re") Nothing [
    _ResolutionError_noSuchBinding>>: "e" ~>
      Strings.concat2 (string "no such binding: ")
        (Core.unName $ project _NoSuchBindingError _NoSuchBindingError_name @@ var "e"),
    _ResolutionError_noSuchPrimitive>>: "e" ~>
      Strings.concat2 (string "no such primitive: ")
        (Core.unName $ project _NoSuchPrimitiveError _NoSuchPrimitiveError_name @@ var "e"),
    _ResolutionError_noMatchingField>>: "e" ~>
      Strings.concat2 (string "no matching field: ")
        (Core.unName $ project _NoMatchingFieldError _NoMatchingFieldError_fieldName @@ var "e"),
    _ResolutionError_other>>: "e" ~>
      Strings.concat2 (string "resolution error: ") (unwrap _OtherResolutionError @@ var "e"),
    _ResolutionError_unexpectedShape>>: "e" ~>
      Strings.concat $ list [
        string "unexpected shape: expected ",
        project _UnexpectedShapeError _UnexpectedShapeError_expected @@ var "e",
        string " but got ",
        project _UnexpectedShapeError _UnexpectedShapeError_actual @@ var "e"]]

subtermPath :: TypedTermDefinition (SubtermPath -> String)
subtermPath = define "subtermPath" $
  doc "Print a subterm path as its steps joined by '/'" $
  "path" ~> Strings.join (string "/") (Lists.map (asTerm subtermStep) (unwrap _SubtermPath @@ var "path"))

subtermStep :: TypedTermDefinition (SubtermStep -> String)
subtermStep = define "subtermStep" $
  doc "Print a subterm step in its round-trippable notation" $
  "step" ~> match _SubtermStep (var "step") Nothing [
    _SubtermStep_annotatedAnnotation>>: constant (string "annotatedAnnotation"),
    _SubtermStep_annotatedBody>>: constant (string "annotatedBody"),
    _SubtermStep_applicationArgument>>: constant (string "applicationArgument"),
    _SubtermStep_applicationFunction>>: constant (string "applicationFunction"),
    _SubtermStep_casesCase>>: "name" ~> Strings.concat2 (string "casesCase:") (Core.unName $ var "name"),
    _SubtermStep_casesDefault>>: constant (string "casesDefault"),
    _SubtermStep_eitherLeft>>: constant (string "eitherLeft"),
    _SubtermStep_eitherRight>>: constant (string "eitherRight"),
    _SubtermStep_injectField>>: "name" ~> Strings.concat2 (string "injectField:") (Core.unName $ var "name"),
    _SubtermStep_lambdaBody>>: constant (string "lambdaBody"),
    _SubtermStep_letBinding>>: "name" ~> Strings.concat2 (string "letBinding:") (Core.unName $ var "name"),
    _SubtermStep_letBody>>: constant (string "letBody"),
    _SubtermStep_listElement>>: "i" ~> Strings.concat2 (string "listElement:") (Literals.printInt32 $ var "i"),
    _SubtermStep_mapEntry>>: "i" ~> Strings.concat2 (string "mapEntry:") (Literals.printInt32 $ var "i"),
    _SubtermStep_optionalGiven>>: constant (string "optionalGiven"),
    _SubtermStep_pairFirst>>: constant (string "pairFirst"),
    _SubtermStep_pairSecond>>: constant (string "pairSecond"),
    _SubtermStep_recordField>>: "name" ~> Strings.concat2 (string "recordField:") (Core.unName $ var "name"),
    _SubtermStep_setElement>>: "i" ~> Strings.concat2 (string "setElement:") (Literals.printInt32 $ var "i"),
    _SubtermStep_typeApplicationBody>>: constant (string "typeApplicationBody"),
    _SubtermStep_typeLambdaBody>>: constant (string "typeLambdaBody"),
    _SubtermStep_wrapBody>>: constant (string "wrapBody")]

typeArityMismatchError :: TypedTermDefinition (TypeArityMismatchError -> String)
typeArityMismatchError = define "typeArityMismatchError" $
  doc "Show a type arity mismatch error as a string" $
  "e" ~>
  "typ" <~ project _TypeArityMismatchError _TypeArityMismatchError_type @@ var "e" $
  "expected" <~ project _TypeArityMismatchError _TypeArityMismatchError_expectedArity @@ var "e" $
  "actual" <~ project _TypeArityMismatchError _TypeArityMismatchError_actualArity @@ var "e" $
  "args" <~ project _TypeArityMismatchError _TypeArityMismatchError_typeArguments @@ var "e" $
  Strings.concat $ list [
    string "type ",
    PrintCore.type_ @@ var "typ",
    string " applied to the wrong number of type arguments (expected ",
    Literals.printInt32 (var "expected"),
    string ", got ",
    Literals.printInt32 (var "actual"),
    string "): ",
    Formatting.showList @@ PrintCore.type_ @@ var "args"]

typeMismatchError :: TypedTermDefinition (TypeMismatchError -> String)
typeMismatchError = define "typeMismatchError" $
  doc "Show a type mismatch error as a string" $
  "e" ~>
  "expected" <~ project _TypeMismatchError _TypeMismatchError_expectedType @@ var "e" $
  "actual" <~ project _TypeMismatchError _TypeMismatchError_actualType @@ var "e" $
  Strings.concat $ list [
    string "type mismatch: expected ",
    PrintCore.type_ @@ var "expected",
    string " but found ",
    PrintCore.type_ @@ var "actual"]

unboundTypeVariablesError :: TypedTermDefinition (UnboundTypeVariablesError -> String)
unboundTypeVariablesError = define "unboundTypeVariablesError" $
  doc "Show an unbound type variables error as a string" $
  "e" ~>
  "vars" <~ project _UnboundTypeVariablesError _UnboundTypeVariablesError_variables @@ var "e" $
  "typ" <~ project _UnboundTypeVariablesError _UnboundTypeVariablesError_type @@ var "e" $
  Strings.concat $ list [
    string "unbound type variables: {",
    Strings.join (string ", ") (Lists.map (reify Core.unName) $ Sets.toList $ var "vars"),
    string "} in type ",
    PrintCore.type_ @@ var "typ"]

unequalTypesError :: TypedTermDefinition (UnequalTypesError -> String)
unequalTypesError = define "unequalTypesError" $
  doc "Show an unequal types error as a string" $
  "e" ~>
  "types" <~ project _UnequalTypesError _UnequalTypesError_types @@ var "e" $
  "desc" <~ project _UnequalTypesError _UnequalTypesError_description @@ var "e" $
  Strings.concat $ list [
    string "unequal types ",
    Formatting.showList @@ PrintCore.type_ @@ var "types",
    string " in ",
    var "desc"]

unificationError :: TypedTermDefinition (UnificationError -> String)
unificationError = define "unificationError" $
  doc "Show a unification error as a string" $
  "e" ~>
  "lt" <~ project _UnificationError _UnificationError_leftType @@ var "e" $
  "rt" <~ project _UnificationError _UnificationError_rightType @@ var "e" $
  "msg" <~ project _UnificationError _UnificationError_message @@ var "e" $
  Strings.concat $ list [
    string "unification error: cannot unify ",
    PrintCore.type_ @@ var "lt",
    string " with ",
    PrintCore.type_ @@ var "rt",
    string ": ",
    var "msg"]

unsupportedTermVariantError :: TypedTermDefinition (UnsupportedTermVariantError -> String)
unsupportedTermVariantError = define "unsupportedTermVariantError" $
  doc "Show an unsupported term variant error as a string" $
  "e" ~>
  Strings.concat2
    (string "unsupported term variant: ")
    (PrintVariants.termVariant @@ (project _UnsupportedTermVariantError _UnsupportedTermVariantError_termVariant @@ var "e"))

untypedLambdaError :: TypedTermDefinition (UntypedLambdaError -> String)
untypedLambdaError = define "untypedLambdaError" $
  doc "Show an untyped lambda error as a string" $
  constant $ string "untyped lambda"

untypedLetBindingError :: TypedTermDefinition (UntypedLetBindingError -> String)
untypedLetBindingError = define "untypedLetBindingError" $
  doc "Show an untyped let binding error as a string" $
  "e" ~>
  "b" <~ project _UntypedLetBindingError _UntypedLetBindingError_binding @@ var "e" $
  Strings.concat2 (string "untyped let binding: ") (PrintCore.binding @@ var "b")
