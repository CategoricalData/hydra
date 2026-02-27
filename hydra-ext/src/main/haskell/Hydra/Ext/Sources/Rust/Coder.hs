-- | Rust code generator in Hydra DSL.
-- This module provides DSL versions of Rust code generation functions for type modules.
-- Only type definitions (records, unions, newtypes) are mapped; term definitions are ignored.

module Hydra.Ext.Sources.Rust.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Rust AST
import qualified Hydra.Ext.Rust.Syntax as R
import qualified Hydra.Ext.Sources.Rust.Syntax as RustSyntax
import qualified Hydra.Ext.Sources.Rust.Serde as RustSerdeSource
import qualified Hydra.Ext.Sources.Rust.Language as RustLanguageSource


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.rust.coder"

module_ :: Module
module_ = Module ns elements
    [moduleNamespace RustSerdeSource.module_, moduleNamespace RustLanguageSource.module_,
      Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns, Lexical.ns, Monads.ns, SerializationSource.ns]
    (RustSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Rust code generator: converts Hydra type modules to Rust source code"
  where
    elements = [
      toBinding standardDerives,
      toBinding rustPath,
      toBinding rustPathSegmented,
      toBinding rustApply1,
      toBinding rustApply2,
      toBinding rustUnit,
      toBinding encodeLiteralType,
      toBinding encodeType,
      toBinding encodeStructField,
      toBinding encodeEnumVariant,
      toBinding encodeTypeDefinition,
      toBinding moduleToRust]

-- =============================================================================
-- Standard derives
-- =============================================================================

standardDerives :: TBinding [String]
standardDerives = def "standardDerives" $
  list $ string <$> ["Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord"]

-- =============================================================================
-- Rust type AST helpers
-- =============================================================================

-- | Construct a simple Rust path type (e.g., "String" -> String)
rustPath :: TBinding (String -> R.Type)
rustPath = def "rustPath" $
  lambda "name" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]]

-- | Construct a Rust path type with multiple segments (e.g., ["num", "BigInt"] -> num::BigInt)
rustPathSegmented :: TBinding ([String] -> R.Type)
rustPathSegmented = def "rustPathSegmented" $
  lambda "segs" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>:
          Lists.map (lambda "s" $
            record R._PathSegment [
              R._PathSegment_name>>: var "s",
              R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit])
          (var "segs")]

-- | Apply a type constructor to one type argument (e.g., Vec<T>)
rustApply1 :: TBinding (String -> R.Type -> R.Type)
rustApply1 = def "rustApply1" $
  lambda "name" $ lambda "arg" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>:
              inject R._GenericArguments R._GenericArguments_angleBracketed $
                record R._AngleBracketedArgs [
                  R._AngleBracketedArgs_args>>: list [
                    inject R._GenericArg R._GenericArg_type (var "arg")]]]]]

-- | Apply a type constructor to two type arguments (e.g., BTreeMap<K, V>)
rustApply2 :: TBinding (String -> R.Type -> R.Type -> R.Type)
rustApply2 = def "rustApply2" $
  lambda "name" $ lambda "arg1" $ lambda "arg2" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>:
              inject R._GenericArguments R._GenericArguments_angleBracketed $
                record R._AngleBracketedArgs [
                  R._AngleBracketedArgs_args>>: list [
                    inject R._GenericArg R._GenericArg_type (var "arg1"),
                    inject R._GenericArg R._GenericArg_type (var "arg2")]]]]]

-- | The Rust unit type ()
rustUnit :: TBinding R.Type
rustUnit = def "rustUnit" $
  inject R._Type R._Type_unit unit

-- =============================================================================
-- Literal type encoding
-- =============================================================================

-- | Encode a Hydra literal type as a Rust type
encodeLiteralType :: TBinding (LiteralType -> R.Type)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      rustApply1 @@ string "Vec" @@ (rustPath @@ string "u8"),
    _LiteralType_boolean>>: constant $
      rustPath @@ string "bool",
    _LiteralType_float>>: lambda "ft" $
      cases _FloatType (var "ft") Nothing [
        _FloatType_bigfloat>>: constant $ rustPath @@ string "f64",
        _FloatType_float32>>: constant $ rustPath @@ string "f32",
        _FloatType_float64>>: constant $ rustPath @@ string "f64"],
    _LiteralType_integer>>: lambda "it" $
      cases _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $ rustPathSegmented @@ list [string "num", string "BigInt"],
        _IntegerType_int8>>: constant $ rustPath @@ string "i8",
        _IntegerType_int16>>: constant $ rustPath @@ string "i16",
        _IntegerType_int32>>: constant $ rustPath @@ string "i32",
        _IntegerType_int64>>: constant $ rustPath @@ string "i64",
        _IntegerType_uint8>>: constant $ rustPath @@ string "u8",
        _IntegerType_uint16>>: constant $ rustPath @@ string "u16",
        _IntegerType_uint32>>: constant $ rustPath @@ string "u32",
        _IntegerType_uint64>>: constant $ rustPath @@ string "u64"],
    _LiteralType_string>>: constant $
      rustPath @@ string "String"]

-- =============================================================================
-- Type encoding
-- =============================================================================

-- | Encode a Hydra type as a Rust syntax type
encodeType :: TBinding (Type -> Flow Graph R.Type)
encodeType = def "encodeType" $
  lambda "t" $
    "typ" <~ (Rewriting.deannotateType @@ var "t") $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $
        encodeType @@ Core.annotatedTypeBody (var "at"),
      _Type_application>>: lambda "at" $
        encodeType @@ Core.applicationTypeFunction (var "at"),
      _Type_unit>>: constant $
        Flows.pure (asTerm rustUnit),
      _Type_literal>>: lambda "lt" $
        Flows.pure (encodeLiteralType @@ var "lt"),
      _Type_list>>: lambda "inner" $
        Flows.map (lambda "enc" $ rustApply1 @@ string "Vec" @@ var "enc")
          (encodeType @@ var "inner"),
      _Type_set>>: lambda "inner" $
        Flows.map (lambda "enc" $ rustApply1 @@ string "BTreeSet" @@ var "enc")
          (encodeType @@ var "inner"),
      _Type_map>>: lambda "mt" $
        "kt" <<~ (encodeType @@ Core.mapTypeKeys (var "mt")) $
        "vt" <<~ (encodeType @@ Core.mapTypeValues (var "mt")) $
          Flows.pure (rustApply2 @@ string "BTreeMap" @@ var "kt" @@ var "vt"),
      _Type_maybe>>: lambda "inner" $
        Flows.map (lambda "enc" $ rustApply1 @@ string "Option" @@ var "enc")
          (encodeType @@ var "inner"),
      _Type_either>>: lambda "et" $
        "lt" <<~ (encodeType @@ Core.eitherTypeLeft (var "et")) $
        "rt" <<~ (encodeType @@ Core.eitherTypeRight (var "et")) $
          Flows.pure (rustApply2 @@ string "Either" @@ var "lt" @@ var "rt"),
      _Type_pair>>: lambda "pt" $
        "ft" <<~ (encodeType @@ Core.pairTypeFirst (var "pt")) $
        "st" <<~ (encodeType @@ Core.pairTypeSecond (var "pt")) $
          Flows.pure (inject R._Type R._Type_tuple $ list [var "ft", var "st"]),
      _Type_function>>: lambda "ft" $
        "dom" <<~ (encodeType @@ Core.functionTypeDomain (var "ft")) $
        "cod" <<~ (encodeType @@ Core.functionTypeCodomain (var "ft")) $
          Flows.pure (rustApply1 @@ string "Box"
            @@ (inject R._Type R._Type_dynTrait $ list [
              inject R._TypeParamBound R._TypeParamBound_trait $
                record R._TypePath [
                  R._TypePath_global>>: boolean False,
                  R._TypePath_segments>>: list [
                    record R._PathSegment [
                      R._PathSegment_name>>: string "Fn",
                      R._PathSegment_arguments>>:
                        inject R._GenericArguments R._GenericArguments_parenthesized $
                          record R._ParenthesizedArgs [
                            R._ParenthesizedArgs_inputs>>: list [var "dom"],
                            R._ParenthesizedArgs_output>>: just (var "cod")]]]]])),
      _Type_record>>: lambda "rt" $
        Flows.pure (rustPath @@ (Formatting.capitalize @@ (Names.localNameOf @@ Core.rowTypeTypeName (var "rt")))),
      _Type_union>>: lambda "rt" $
        Flows.pure (rustPath @@ (Formatting.capitalize @@ (Names.localNameOf @@ Core.rowTypeTypeName (var "rt")))),
      _Type_wrap>>: lambda "wt" $
        Flows.pure (rustPath @@ (Formatting.capitalize @@ (Names.localNameOf @@ Core.wrappedTypeTypeName (var "wt")))),
      _Type_variable>>: lambda "name" $
        Flows.pure (rustPath @@ (Formatting.capitalize @@ Core.unName (var "name"))),
      _Type_forall>>: lambda "fa" $
        encodeType @@ Core.forallTypeBody (var "fa")]

-- =============================================================================
-- Struct field encoding
-- =============================================================================

-- | Encode a Hydra record field as a Rust struct field
encodeStructField :: TBinding (FieldType -> Flow Graph R.StructField)
encodeStructField = def "encodeStructField" $
  lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "sftyp" <<~ (encodeType @@ var "ftyp") $
      Flows.pure (record R._StructField [
        R._StructField_name>>: Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ var "fname"),
        R._StructField_type>>: var "sftyp",
        R._StructField_public>>: boolean True,
        R._StructField_doc>>: nothing])

-- =============================================================================
-- Enum variant encoding
-- =============================================================================

-- | Encode a Hydra union field as a Rust enum variant
encodeEnumVariant :: TBinding (FieldType -> Flow Graph R.EnumVariant)
encodeEnumVariant = def "encodeEnumVariant" $
  lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "dtyp" <~ (Rewriting.deannotateType @@ var "ftyp") $
    "isUnit" <~ (cases _Type (var "dtyp") (Just $ boolean False) [
      _Type_unit>>: constant $ boolean True,
      _Type_record>>: lambda "rt" $
        Lists.null (Core.rowTypeFields (var "rt"))]) $
    Logic.ifElse (var "isUnit")
      -- Unit variant
      (Flows.pure (record R._EnumVariant [
        R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
        R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_unit unit,
        R._EnumVariant_doc>>: nothing]))
      -- Non-unit variant: check if it's a record (struct variant) or other (tuple variant)
      (cases _Type (var "dtyp") (Just $
          -- Default: tuple variant with single element
          "sftyp" <<~ (encodeType @@ var "ftyp") $
            Flows.pure (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_tuple $ list [var "sftyp"],
              R._EnumVariant_doc>>: nothing]))
        [_Type_record>>: lambda "rt" $
          "sfields" <<~ (Flows.mapList encodeStructField (Core.rowTypeFields (var "rt"))) $
            Flows.pure (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_struct (var "sfields"),
              R._EnumVariant_doc>>: nothing])])

-- =============================================================================
-- Type definition encoding
-- =============================================================================

-- | Encode a Hydra type definition as a Rust item
encodeTypeDefinition :: TBinding (TypeDefinition -> Flow Graph R.ItemWithComments)
encodeTypeDefinition = def "encodeTypeDefinition" $
  lambda "tdef" $
    "name" <~ Module.typeDefinitionName (var "tdef") $
    "typ" <~ Module.typeDefinitionType (var "tdef") $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    -- Filter free type variables to unqualified names only (type parameters, not type references)
    "freeVars" <~ (Lists.filter
      (lambda "v" $ Lists.null (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "v")))))
      (Sets.toList (Rewriting.freeVariablesInType @@ var "typ"))) $
    "generics" <~ (Lists.map (lambda "v" $
      record R._GenericParam [
        R._GenericParam_name>>: Formatting.capitalize @@ Core.unName (var "v"),
        R._GenericParam_bounds>>: list ([] :: [TTerm R.TypeParamBound])])
      (var "freeVars")) $
    "dtyp" <~ (Rewriting.deannotateType @@ var "typ") $
    Monads.withTrace @@ (Strings.cat2 (string "type ") (Core.unName (var "name"))) @@
    ("item" <<~ (cases _Type (var "dtyp") (Just $
        -- Fallback: type alias
        "styp" <<~ (encodeType @@ var "typ") $
          Flows.pure (inject R._Item R._Item_typeAlias $
            record R._TypeAlias [
              R._TypeAlias_name>>: var "lname",
              R._TypeAlias_generics>>: var "generics",
              R._TypeAlias_type>>: var "styp",
              R._TypeAlias_public>>: boolean True,
              R._TypeAlias_doc>>: nothing]))
      [_Type_record>>: lambda "rt" $
        "sfields" <<~ (Flows.mapList encodeStructField (Core.rowTypeFields (var "rt"))) $
          Flows.pure (inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "lname",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_named (var "sfields"),
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing]),
      _Type_union>>: lambda "rt" $
        "variants" <<~ (Flows.mapList encodeEnumVariant (Core.rowTypeFields (var "rt"))) $
          Flows.pure (inject R._Item R._Item_enum $
            record R._EnumDef [
              R._EnumDef_name>>: var "lname",
              R._EnumDef_generics>>: var "generics",
              R._EnumDef_whereClause>>: nothing,
              R._EnumDef_variants>>: var "variants",
              R._EnumDef_derives>>: asTerm standardDerives,
              R._EnumDef_public>>: boolean True,
              R._EnumDef_doc>>: nothing]),
      _Type_wrap>>: lambda "wt" $
        "styp" <<~ (encodeType @@ Core.wrappedTypeBody (var "wt")) $
          Flows.pure (inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "lname",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_tuple $ list [
                record R._TupleField [
                  R._TupleField_type>>: var "styp",
                  R._TupleField_public>>: boolean True]],
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing])]) $
    Flows.pure (record R._ItemWithComments [
      R._ItemWithComments_doc>>: nothing,
      R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
      R._ItemWithComments_item>>: var "item"]))

-- =============================================================================
-- Module entry point
-- =============================================================================

-- | Convert a Hydra module to a map of file paths to Rust source code strings.
-- Only type definitions are mapped; term definitions are ignored.
moduleToRust :: TBinding (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
moduleToRust = def "moduleToRust" $
  lambda "mod" $ lambda "defs" $
    Monads.withTrace @@ (Strings.cat2 (string "encode Rust module: ") (Core.unNamespace $ Module.moduleNamespace (var "mod"))) @@
    ("typeDefs" <~ (Pairs.first (Schemas.partitionDefinitions @@ var "defs")) $
    "items" <<~ (Flows.mapList encodeTypeDefinition (var "typeDefs")) $
    "crate" <~ (record R._Crate [R._Crate_items>>: var "items"]) $
    "code" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (RustSerdeSource.crateToExpr @@ var "crate"))) $
    "filePath" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "rs") @@ (Module.moduleNamespace (var "mod"))) $
      Flows.pure (Maps.singleton (var "filePath") (var "code")))
