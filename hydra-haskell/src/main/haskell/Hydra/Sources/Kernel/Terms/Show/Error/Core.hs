
module Hydra.Sources.Kernel.Terms.Show.Error.Core where

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


ns :: Namespace
ns = Namespace "hydra.show.error.core"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, ShowMeta.ns]
    kernelTypesNamespaces $
    Just "String representations of hydra.errors.core types"
  where
   elements = [
     toBinding duplicateBindingError,
     toBinding duplicateFieldError,
     toBinding invalidTermError,
     toBinding undefinedFieldError,
     toBinding undefinedTermError,
     toBinding undefinedTypeError,
     toBinding unexpectedTermVariantError,
     toBinding unexpectedTypeVariantError]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

duplicateBindingError :: TBinding (DuplicateBindingError -> String)
duplicateBindingError = define "duplicateBindingError" $
  doc "Show a duplicate binding error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate binding: ",
    Core.unName $ project _DuplicateBindingError _DuplicateBindingError_name @@ var "e"]

duplicateFieldError :: TBinding (DuplicateFieldError -> String)
duplicateFieldError = define "duplicateFieldError" $
  doc "Show a duplicate field error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate field: ",
    Core.unName $ project _DuplicateFieldError _DuplicateFieldError_name @@ var "e"]

invalidTermError :: TBinding (InvalidTermError -> String)
invalidTermError = define "invalidTermError" $
  doc "Show an invalid term error as a string" $
  "e" ~> Strings.cat2 (string "invalid term: ") $
    cases _InvalidTermError (var "e") Nothing [
      _InvalidTermError_duplicateBinding>>: duplicateBindingError,
      _InvalidTermError_duplicateField>>: duplicateFieldError]

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
