
module Hydra.Sources.Kernel.Terms.Show.Error where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Meta as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting


ns :: Namespace
ns = Namespace "hydra.show.error"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, ShowMeta.ns, ShowTyping.ns, Formatting.ns]
    kernelTypesNamespaces $
    Just "String representations of hydra.error types"
  where
   elements = [
     toBinding checkingError,
     toBinding decodingError,
     toBinding duplicateBindingError,
     toBinding duplicateFieldError,
     toBinding error_,
     toBinding incorrectUnificationError,
     toBinding notAForallTypeError,
     toBinding notAFunctionTypeError,
     toBinding otherError,
     toBinding typeArityMismatchError,
     toBinding typeMismatchError,
     toBinding unboundTypeVariablesError,
     toBinding undefinedFieldError,
     toBinding undefinedTermError,
     toBinding undefinedTypeError,
     toBinding unequalTypesError,
     toBinding unexpectedTermVariantError,
     toBinding unexpectedTypeVariantError,
     toBinding unificationError,
     toBinding unsupportedTermVariantError,
     toBinding untypedLambdaError,
     toBinding untypedLetBindingError]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

checkingError :: TBinding (CheckingError -> String)
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

decodingError :: TBinding (DecodingError -> String)
decodingError = define "decodingError" $
  doc "Show a decoding error as a string" $
  "de" ~> Strings.cat2 (string "decoding error: ") (unwrap _DecodingError @@ var "de")

duplicateBindingError :: TBinding (DuplicateBindingError -> String)
duplicateBindingError = define "duplicateBindingError" $
  doc "Show a duplicate binding error as a string" $
  "e" ~> Strings.cat2
    (string "duplicate binding: ")
    (Core.unName $ project _DuplicateBindingError _DuplicateBindingError_name @@ var "e")

duplicateFieldError :: TBinding (DuplicateFieldError -> String)
duplicateFieldError = define "duplicateFieldError" $
  doc "Show a duplicate field error as a string" $
  "e" ~> Strings.cat2
    (string "duplicate field: ")
    (Core.unName $ project _DuplicateFieldError _DuplicateFieldError_name @@ var "e")

error_ :: TBinding (Error -> String)
error_ = define "error" $
  doc "Show an error as a string" $
  "e" ~> cases _Error (var "e") Nothing [
    _Error_checking>>: checkingError,
    _Error_decoding>>: decodingError,
    _Error_duplicateBinding>>: duplicateBindingError,
    _Error_duplicateField>>: duplicateFieldError,
    _Error_other>>: otherError,
    _Error_undefinedField>>: undefinedFieldError,
    _Error_undefinedTerm>>: undefinedTermError,
    _Error_undefinedType>>: undefinedTypeError,
    _Error_unexpectedTermVariant>>: unexpectedTermVariantError,
    _Error_unexpectedTypeVariant>>: unexpectedTypeVariantError,
    _Error_unification>>: unificationError]

incorrectUnificationError :: TBinding (IncorrectUnificationError -> String)
incorrectUnificationError = define "incorrectUnificationError" $
  doc "Show an incorrect unification error as a string" $
  "e" ~>
  "subst" <~ project _IncorrectUnificationError _IncorrectUnificationError_substitution @@ var "e" $
  Strings.cat2 (string "incorrect unification: ") (ShowTyping.typeSubst @@ var "subst")

notAForallTypeError :: TBinding (NotAForallTypeError -> String)
notAForallTypeError = define "notAForallTypeError" $
  doc "Show a not-a-forall-type error as a string" $
  "e" ~>
  "typ" <~ project _NotAForallTypeError _NotAForallTypeError_type @@ var "e" $
  "args" <~ project _NotAForallTypeError _NotAForallTypeError_typeArguments @@ var "e" $
  Strings.cat $ list [
    string "not a forall type: ",
    ShowCore.type_ @@ var "typ",
    string ". Trying to apply ",
    Literals.showInt32 (Lists.length $ var "args"),
    string " type argument(s): ",
    Formatting.showList @@ ShowCore.type_ @@ var "args"]

notAFunctionTypeError :: TBinding (NotAFunctionTypeError -> String)
notAFunctionTypeError = define "notAFunctionTypeError" $
  doc "Show a not-a-function-type error as a string" $
  "e" ~>
  "typ" <~ project _NotAFunctionTypeError _NotAFunctionTypeError_type @@ var "e" $
  Strings.cat2 (string "not a function type: ") (ShowCore.type_ @@ var "typ")

otherError :: TBinding (OtherError -> String)
otherError = define "otherError" $
  doc "Show an other error as a string" $
  "oe" ~> unwrap _OtherError @@ var "oe"

typeArityMismatchError :: TBinding (TypeArityMismatchError -> String)
typeArityMismatchError = define "typeArityMismatchError" $
  doc "Show a type arity mismatch error as a string" $
  "e" ~>
  "typ" <~ project _TypeArityMismatchError _TypeArityMismatchError_type @@ var "e" $
  "expected" <~ project _TypeArityMismatchError _TypeArityMismatchError_expectedArity @@ var "e" $
  "actual" <~ project _TypeArityMismatchError _TypeArityMismatchError_actualArity @@ var "e" $
  "args" <~ project _TypeArityMismatchError _TypeArityMismatchError_typeArguments @@ var "e" $
  Strings.cat $ list [
    string "type ",
    ShowCore.type_ @@ var "typ",
    string " applied to the wrong number of type arguments (expected ",
    Literals.showInt32 (var "expected"),
    string ", got ",
    Literals.showInt32 (var "actual"),
    string "): ",
    Formatting.showList @@ ShowCore.type_ @@ var "args"]

typeMismatchError :: TBinding (TypeMismatchError -> String)
typeMismatchError = define "typeMismatchError" $
  doc "Show a type mismatch error as a string" $
  "e" ~>
  "expected" <~ project _TypeMismatchError _TypeMismatchError_expectedType @@ var "e" $
  "actual" <~ project _TypeMismatchError _TypeMismatchError_actualType @@ var "e" $
  Strings.cat $ list [
    string "type mismatch: expected ",
    ShowCore.type_ @@ var "expected",
    string " but found ",
    ShowCore.type_ @@ var "actual"]

unboundTypeVariablesError :: TBinding (UnboundTypeVariablesError -> String)
unboundTypeVariablesError = define "unboundTypeVariablesError" $
  doc "Show an unbound type variables error as a string" $
  "e" ~>
  "vars" <~ project _UnboundTypeVariablesError _UnboundTypeVariablesError_variables @@ var "e" $
  "typ" <~ project _UnboundTypeVariablesError _UnboundTypeVariablesError_type @@ var "e" $
  Strings.cat $ list [
    string "unbound type variables: {",
    Strings.intercalate (string ", ") (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "vars"),
    string "} in type ",
    ShowCore.type_ @@ var "typ"]

undefinedFieldError :: TBinding (UndefinedFieldError -> String)
undefinedFieldError = define "undefinedFieldError" $
  doc "Show an undefined field error as a string" $
  "e" ~>
  "fname" <~ project _UndefinedFieldError _UndefinedFieldError_fieldName @@ var "e" $
  "tname" <~ project _UndefinedFieldError _UndefinedFieldError_typeName @@ var "e" $
  Strings.cat $ list [
    string "no such field \"",
    Core.unName $ var "fname",
    string "\" in type \"",
    Core.unName $ var "tname",
    string "\""]

undefinedTermError :: TBinding (UndefinedTermError -> String)
undefinedTermError = define "undefinedTermError" $
  doc "Show an undefined term error as a string" $
  "e" ~>
  Strings.cat2
    (string "undefined term: ")
    (Core.unName $ project _UndefinedTermError _UndefinedTermError_name @@ var "e")

undefinedTypeError :: TBinding (UndefinedTypeError -> String)
undefinedTypeError = define "undefinedTypeError" $
  doc "Show an undefined type error as a string" $
  "e" ~>
  Strings.cat2
    (string "undefined type: ")
    (Core.unName $ project _UndefinedTypeError _UndefinedTypeError_name @@ var "e")

unequalTypesError :: TBinding (UnequalTypesError -> String)
unequalTypesError = define "unequalTypesError" $
  doc "Show an unequal types error as a string" $
  "e" ~>
  "types" <~ project _UnequalTypesError _UnequalTypesError_types @@ var "e" $
  "desc" <~ project _UnequalTypesError _UnequalTypesError_description @@ var "e" $
  Strings.cat $ list [
    string "unequal types ",
    Formatting.showList @@ ShowCore.type_ @@ var "types",
    string " in ",
    var "desc"]

unexpectedTermVariantError :: TBinding (UnexpectedTermVariantError -> String)
unexpectedTermVariantError = define "unexpectedTermVariantError" $
  doc "Show an unexpected term variant error as a string" $
  "e" ~>
  "expected" <~ project _UnexpectedTermVariantError _UnexpectedTermVariantError_expectedVariant @@ var "e" $
  "actual" <~ project _UnexpectedTermVariantError _UnexpectedTermVariantError_actualTerm @@ var "e" $
  Strings.cat $ list [
    string "expected ",
    ShowMeta.termVariant @@ var "expected",
    string " term but found ",
    ShowCore.term @@ var "actual"]

unexpectedTypeVariantError :: TBinding (UnexpectedTypeVariantError -> String)
unexpectedTypeVariantError = define "unexpectedTypeVariantError" $
  doc "Show an unexpected type variant error as a string" $
  "e" ~>
  "expected" <~ project _UnexpectedTypeVariantError _UnexpectedTypeVariantError_expectedVariant @@ var "e" $
  "actual" <~ project _UnexpectedTypeVariantError _UnexpectedTypeVariantError_actualType @@ var "e" $
  Strings.cat $ list [
    string "expected ",
    ShowMeta.typeVariant @@ var "expected",
    string " type but found ",
    ShowCore.type_ @@ var "actual"]

unificationError :: TBinding (UnificationError -> String)
unificationError = define "unificationError" $
  doc "Show a unification error as a string" $
  "e" ~>
  "lt" <~ project _UnificationError _UnificationError_leftType @@ var "e" $
  "rt" <~ project _UnificationError _UnificationError_rightType @@ var "e" $
  "msg" <~ project _UnificationError _UnificationError_message @@ var "e" $
  Strings.cat $ list [
    string "unification error: cannot unify ",
    ShowCore.type_ @@ var "lt",
    string " with ",
    ShowCore.type_ @@ var "rt",
    string ": ",
    var "msg"]

unsupportedTermVariantError :: TBinding (UnsupportedTermVariantError -> String)
unsupportedTermVariantError = define "unsupportedTermVariantError" $
  doc "Show an unsupported term variant error as a string" $
  "e" ~>
  Strings.cat2
    (string "unsupported term variant: ")
    (ShowMeta.termVariant @@ (project _UnsupportedTermVariantError _UnsupportedTermVariantError_termVariant @@ var "e"))

untypedLambdaError :: TBinding (UntypedLambdaError -> String)
untypedLambdaError = define "untypedLambdaError" $
  doc "Show an untyped lambda error as a string" $
  constant $ string "untyped lambda"

untypedLetBindingError :: TBinding (UntypedLetBindingError -> String)
untypedLetBindingError = define "untypedLetBindingError" $
  doc "Show an untyped let binding error as a string" $
  "e" ~>
  "b" <~ project _UntypedLetBindingError _UntypedLetBindingError_binding @@ var "e" $
  Strings.cat2 (string "untyped let binding: ") (ShowCore.binding @@ var "b")
