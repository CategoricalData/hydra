
module Hydra.Sources.Kernel.Terms.Print.Errors where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
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
ns = ModuleName "hydra.print.errors"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([PrintCore.ns, PrintErrorCore.ns, PrintVariants.ns, PrintTyping.ns, Formatting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.error types")}
  where
   definitions = [
     toDefinition checkingError,
     toDefinition decodingError,
     toDefinition error_,
     toDefinition incorrectUnificationError,
     toDefinition notAForallTypeError,
     toDefinition notAFunctionTypeError,
     toDefinition otherError,
     toDefinition resolutionError,
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
  "ce" ~> cases _CheckingError (var "ce") Nothing [
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
  "e" ~> cases _Error (var "e") Nothing [
    _Error_checking>>: checkingError,
    _Error_decoding>>: decodingError,
    _Error_duplicateBinding>>: PrintErrorCore.duplicateBindingError,
    _Error_duplicateField>>: PrintErrorCore.duplicateFieldError,
    _Error_extraction>>: constant $ string "extraction error",
    _Error_inference>>: constant $ string "inference error",
    _Error_other>>: otherError,
    _Error_resolution>>: resolutionError,
    _Error_undefinedField>>: PrintErrorCore.undefinedFieldError,
    _Error_undefinedTermVariable>>: PrintErrorCore.undefinedTermVariableError,
    _Error_untypedTermVariable>>: PrintErrorCore.untypedTermVariableError,
    _Error_unexpectedTermVariant>>: PrintErrorCore.unexpectedTermVariantError,
    _Error_unexpectedTypeVariant>>: PrintErrorCore.unexpectedTypeVariantError,
    _Error_unification>>: unificationError]

incorrectUnificationError :: TypedTermDefinition (IncorrectUnificationError -> String)
incorrectUnificationError = define "incorrectUnificationError" $
  doc "Show an incorrect unification error as a string" $
  "e" ~>
  "subst" <~ project _IncorrectUnificationError _IncorrectUnificationError_substitution @@ var "e" $
  Strings.concat2 (string "incorrect unification: ") (PrintTyping.typeSubst @@ var "subst")

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
    Literals.showInt32 (Lists.length $ var "args"),
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
  "re" ~> cases _ResolutionError (var "re") Nothing [
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
    Literals.showInt32 (var "expected"),
    string ", got ",
    Literals.showInt32 (var "actual"),
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
