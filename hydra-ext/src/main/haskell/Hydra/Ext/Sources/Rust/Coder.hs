-- | Rust code generator in Hydra DSL.
-- This module provides DSL versions of Rust code generation functions.
-- Type definitions are mapped to structs/enums/newtypes; term definitions are mapped to functions.

module Hydra.Ext.Sources.Rust.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
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


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.rust.coder"

module_ :: Module
module_ = Module ns definitions
    [moduleNamespace RustSerdeSource.module_, moduleNamespace RustLanguageSource.module_,
      Formatting.ns, Names.ns, Strip.ns, Variables.ns, Environment.ns, Lexical.ns, SerializationSource.ns]
    (RustSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Rust code generator: converts Hydra type and term modules to Rust source code"
  where
    definitions = [
      toDefinition standardDerives,
      toDefinition rustPath,
      toDefinition rustPathSegmented,
      toDefinition rustApply1,
      toDefinition rustApply2,
      toDefinition rustUnit,
      toDefinition rustExprPath,
      toDefinition rustCall,
      toDefinition rustBlock,
      toDefinition rustLetStmt,
      toDefinition rustClosure,
      toDefinition encodeLiteralType,
      toDefinition encodeLiteral,
      toDefinition encodeType,
      toDefinition encodeTerm,
      toDefinition encodeFunction,
      toDefinition encodeElimination,
      toDefinition encodeStructField,
      toDefinition encodeEnumVariant,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTermDefinition,
      toDefinition moduleToRust]

-- =============================================================================
-- Standard derives
-- =============================================================================

standardDerives :: TTermDefinition [String]
standardDerives = def "standardDerives" $
  list $ string <$> ["Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord"]

-- =============================================================================
-- Rust type AST helpers
-- =============================================================================

-- | Construct a simple Rust path type (e.g., "String" -> String)
rustPath :: TTermDefinition (String -> R.Type)
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
rustPathSegmented :: TTermDefinition ([String] -> R.Type)
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
rustApply1 :: TTermDefinition (String -> R.Type -> R.Type)
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
rustApply2 :: TTermDefinition (String -> R.Type -> R.Type -> R.Type)
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
rustUnit :: TTermDefinition R.Type
rustUnit = def "rustUnit" $
  inject R._Type R._Type_unit unit

-- =============================================================================
-- Rust expression AST helpers
-- =============================================================================

-- | Variable reference as a path expression
rustExprPath :: TTermDefinition (String -> R.Expression)
rustExprPath = def "rustExprPath" $
  lambda "name" $
    inject R._Expression R._Expression_path $
      record R._ExprPath [
        R._ExprPath_global>>: boolean False,
        R._ExprPath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]]

-- | Function call expression
rustCall :: TTermDefinition (R.Expression -> [R.Expression] -> R.Expression)
rustCall = def "rustCall" $
  lambda "fun" $ lambda "args" $
    inject R._Expression R._Expression_call $
      record R._CallExpr [
        R._CallExpr_function>>: var "fun",
        R._CallExpr_args>>: var "args"]

-- | Block expression with statements and trailing expression
rustBlock :: TTermDefinition ([R.Statement] -> R.Expression -> R.Expression)
rustBlock = def "rustBlock" $
  lambda "stmts" $ lambda "expr" $
    inject R._Expression R._Expression_block $
      record R._Block [
        R._Block_statements>>: var "stmts",
        R._Block_expression>>: just (var "expr")]

-- | Let statement: let name = expr;
rustLetStmt :: TTermDefinition (String -> R.Expression -> R.Statement)
rustLetStmt = def "rustLetStmt" $
  lambda "name" $ lambda "expr" $
    inject R._Statement R._Statement_let $
      record R._LetStatement [
        R._LetStatement_pattern>>:
          inject R._Pattern R._Pattern_identifier $
            record R._IdentifierPattern [
              R._IdentifierPattern_name>>: var "name",
              R._IdentifierPattern_mutable>>: boolean False,
              R._IdentifierPattern_atPattern>>: nothing],
        R._LetStatement_mutable>>: boolean False,
        R._LetStatement_type>>: nothing,
        R._LetStatement_init>>: just (var "expr")]

-- | Closure expression: |params| body
rustClosure :: TTermDefinition ([String] -> R.Expression -> R.Expression)
rustClosure = def "rustClosure" $
  lambda "params" $ lambda "body" $
    inject R._Expression R._Expression_closure $
      record R._ClosureExpr [
        R._ClosureExpr_move>>: boolean False,
        R._ClosureExpr_params>>:
          Lists.map (lambda "p" $
            record R._ClosureParam [
              R._ClosureParam_pattern>>:
                inject R._Pattern R._Pattern_identifier $
                  record R._IdentifierPattern [
                    R._IdentifierPattern_name>>: var "p",
                    R._IdentifierPattern_mutable>>: boolean False,
                    R._IdentifierPattern_atPattern>>: nothing],
              R._ClosureParam_type>>: nothing])
          (var "params"),
        R._ClosureExpr_returnType>>: nothing,
        R._ClosureExpr_body>>: var "body"]

-- =============================================================================
-- Literal type encoding
-- =============================================================================

-- | Encode a Hydra literal type as a Rust type
encodeLiteralType :: TTermDefinition (LiteralType -> R.Type)
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
-- Literal value encoding
-- =============================================================================

-- | Encode a Hydra literal value as a Rust expression
encodeLiteral :: TTermDefinition (Literal -> R.Expression)
encodeLiteral = def "encodeLiteral" $
  lambda "lit" $ cases _Literal (var "lit") Nothing [
    _Literal_boolean>>: lambda "b" $
      inject R._Expression R._Expression_literal $
        inject R._Literal R._Literal_bool (var "b"),
    _Literal_string>>: lambda "s" $
      inject R._Expression R._Expression_literal $
        inject R._Literal R._Literal_string (var "s"),
    _Literal_float>>: lambda "fv" $
      cases _FloatValue (var "fv") Nothing [
        _FloatValue_float32>>: lambda "f" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_float $
              record R._FloatLiteral [
                R._FloatLiteral_value>>: Literals.bigfloatToFloat64 (Literals.float32ToBigfloat (var "f")),
                R._FloatLiteral_suffix>>: just (string "f32")],
        _FloatValue_float64>>: lambda "f" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_float $
              record R._FloatLiteral [
                R._FloatLiteral_value>>: var "f",
                R._FloatLiteral_suffix>>: nothing],
        _FloatValue_bigfloat>>: lambda "f" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_float $
              record R._FloatLiteral [
                R._FloatLiteral_value>>: Literals.bigfloatToFloat64 (var "f"),
                R._FloatLiteral_suffix>>: nothing]],
    _Literal_integer>>: lambda "iv" $
      cases _IntegerValue (var "iv") Nothing [
        _IntegerValue_int8>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int8ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i8")],
        _IntegerValue_int16>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int16ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i16")],
        _IntegerValue_int32>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int32ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i32")],
        _IntegerValue_int64>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int64ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i64")],
        _IntegerValue_uint8>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint8ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u8")],
        _IntegerValue_uint16>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint16ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u16")],
        _IntegerValue_uint32>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint32ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u32")],
        _IntegerValue_uint64>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint64ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u64")],
        _IntegerValue_bigint>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: var "i",
                R._IntegerLiteral_suffix>>: nothing]]]

-- =============================================================================
-- Type encoding
-- =============================================================================

-- | Encode a Hydra type as a Rust syntax type
encodeType :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) R.Type)
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
      _Type_application>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at"),
      _Type_unit>>: constant $
        right (asTerm rustUnit),
      _Type_void>>: constant $
        right (asTerm rustUnit),
      _Type_literal>>: lambda "lt" $
        right (encodeLiteralType @@ var "lt"),
      _Type_list>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "Vec" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_set>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "BTreeSet" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_map>>: lambda "mt" $
        "kt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeKeys (var "mt")) $
        "vt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeValues (var "mt")) $
          right (rustApply2 @@ string "BTreeMap" @@ var "kt" @@ var "vt"),
      _Type_maybe>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "Option" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_either>>: lambda "et" $
        "lt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeLeft (var "et")) $
        "rt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeRight (var "et")) $
          right (rustApply2 @@ string "Either" @@ var "lt" @@ var "rt"),
      _Type_pair>>: lambda "pt" $
        "ft" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeFirst (var "pt")) $
        "st" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeSecond (var "pt")) $
          right (inject R._Type R._Type_tuple $ list [var "ft", var "st"]),
      _Type_function>>: lambda "ft" $
        "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
        "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
          right (rustApply1 @@ string "Box"
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
      _Type_record>>: lambda "_" $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx"),
      _Type_union>>: lambda "_" $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx"),
      _Type_wrap>>: lambda "_" $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous wrap type")) (var "cx"),
      _Type_variable>>: lambda "name" $
        right (rustPath @@ (Formatting.capitalize @@ Core.unName (var "name"))),
      _Type_forall>>: lambda "fa" $
        encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]

-- =============================================================================
-- Term encoding
-- =============================================================================

-- | Encode a Hydra term as a Rust expression
encodeTerm :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) R.Expression)
encodeTerm = def "encodeTerm" $
  "cx" ~> "g" ~> lambda "term" $
    cases _Term (var "term") (Just $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "unexpected term variant") (var "cx"))
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.annotatedTermBody (var "at"),
     _Term_application>>: lambda "app" $
       "fun" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.applicationFunction (var "app")) $
       "arg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.applicationArgument (var "app")) $
         right (rustCall @@ var "fun" @@ list [var "arg"]),
     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $
           "sl" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "l") $
             right (rustCall @@ (rustExprPath @@ string "Left") @@ list [var "sl"]))
         (lambda "r" $
           "sr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "r") $
             right (rustCall @@ (rustExprPath @@ string "Right") @@ list [var "sr"]))
         (var "e"),
     _Term_function>>: lambda "fun" $
       encodeFunction @@ var "cx" @@ var "g" @@ var "fun",
     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       "stmts" <<~ (Eithers.mapList
         (lambda "b" $
           "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.bindingName (var "b"))) $
           "bval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
             right (rustLetStmt @@ var "bname" @@ var "bval"))
         (var "bindings")) $
       "bodyExpr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "body") $
         right (rustBlock @@ var "stmts" @@ var "bodyExpr"),
     _Term_list>>: lambda "els" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "els")) $
         right (rustCall @@ (rustExprPath @@ string "Vec::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "sels")]),
     _Term_literal>>: lambda "lit" $
       right (encodeLiteral @@ var "lit"),
     _Term_map>>: lambda "m" $
       "pairs" <<~ (Eithers.mapList
         (lambda "entry" $
           "k" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "entry")) $
           "v" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "entry")) $
             right (inject R._Expression R._Expression_tuple $ list [var "k", var "v"]))
         (Maps.toList (var "m"))) $
         right (rustCall @@ (rustExprPath @@ string "BTreeMap::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "pairs")]),
     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         (right (rustExprPath @@ string "None"))
         (lambda "val" $
           "sval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "val") $
             right (rustCall @@ (rustExprPath @@ string "Some") @@ list [var "sval"])),
     _Term_pair>>: lambda "p" $
       "f" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "p")) $
       "s" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "p")) $
         right (inject R._Expression R._Expression_tuple $ list [var "f", var "s"]),
     _Term_record>>: lambda "rec" $
       "rname" <~ Core.recordTypeName (var "rec") $
       "fields" <~ Core.recordFields (var "rec") $
       "sfields" <<~ (Eithers.mapList
         (lambda "f" $
           "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldName (var "f"))) $
           "fval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.fieldTerm (var "f")) $
             right (record R._FieldValue [
               R._FieldValue_name>>: var "fname",
               R._FieldValue_value>>: just (var "fval")]))
         (var "fields")) $
         right (inject R._Expression R._Expression_struct $
           record R._StructExpr [
             R._StructExpr_path>>:
               record R._ExprPath [
                 R._ExprPath_global>>: boolean False,
                 R._ExprPath_segments>>: list [
                   record R._PathSegment [
                     R._PathSegment_name>>: Formatting.capitalize @@ (Names.localNameOf @@ var "rname"),
                     R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
             R._StructExpr_fields>>: var "sfields",
             R._StructExpr_rest>>: nothing]),
     _Term_set>>: lambda "s" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (Sets.toList (var "s"))) $
         right (rustCall @@ (rustExprPath @@ string "BTreeSet::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "sels")]),
     _Term_union>>: lambda "inj" $
       "tname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ Core.injectionTypeName (var "inj"))) $
       "field" <~ Core.injectionField (var "inj") $
       "fname" <~ (Formatting.capitalize @@ Core.unName (Core.fieldName (var "field"))) $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Strip.deannotateTerm @@ var "fterm") $
       "isUnit" <~ (cases _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.null (Core.recordFields (var "rt"))]) $
       Logic.ifElse (var "isUnit")
         (right (rustExprPath @@ Strings.cat2 (Strings.cat2 (var "tname") (string "::")) (var "fname")))
         ("sval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fterm") $
           right (rustCall @@ (rustExprPath @@ Strings.cat2 (Strings.cat2 (var "tname") (string "::")) (var "fname")) @@ list [var "sval"])),
     _Term_unit>>: constant $
       right (inject R._Expression R._Expression_tuple $ list ([] :: [TTerm R.Expression])),
     _Term_variable>>: lambda "name" $
       right (rustExprPath @@ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (var "name")))),
     _Term_wrap>>: lambda "wt" $
       "tname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ Core.wrappedTermTypeName (var "wt"))) $
       "inner" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.wrappedTermBody (var "wt")) $
         right (rustCall @@ (rustExprPath @@ var "tname") @@ list [var "inner"])]

-- =============================================================================
-- Function encoding
-- =============================================================================

-- | Encode a Hydra function as a Rust expression
encodeFunction :: TTermDefinition (Context -> Graph -> Function -> Either (InContext Error) R.Expression)
encodeFunction = def "encodeFunction" $
  "cx" ~> "g" ~> lambda "fun" $
    cases _Function (var "fun") Nothing [
      _Function_lambda>>: lambda "lam" $
        "param" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.lambdaParameter (var "lam"))) $
        "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.lambdaBody (var "lam")) $
          right (rustClosure @@ list [var "param"] @@ var "body"),
      _Function_primitive>>: lambda "name" $
        right (rustExprPath @@ Core.unName (var "name")),
      _Function_elimination>>: lambda "elim" $
        encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@ nothing]

-- =============================================================================
-- Elimination encoding
-- =============================================================================

-- | Encode a Hydra elimination as a Rust expression.
-- Takes an optional argument for applied eliminations.
encodeElimination :: TTermDefinition (Context -> Graph -> Elimination -> Maybe Term -> Either (InContext Error) R.Expression)
encodeElimination = def "encodeElimination" $
  "cx" ~> "g" ~> lambda "elim" $ lambda "marg" $
    cases _Elimination (var "elim") Nothing [
      _Elimination_record>>: lambda "proj" $
        "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.projectionField (var "proj"))) $
        Maybes.cases (var "marg")
          -- Unapplied projection: |v| v.field
          (right (rustClosure @@ list [string "v"] @@
            (inject R._Expression R._Expression_fieldAccess $
              record R._FieldAccessExpr [
                R._FieldAccessExpr_object>>: rustExprPath @@ string "v",
                R._FieldAccessExpr_field>>: var "fname"])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (inject R._Expression R._Expression_fieldAccess $
                record R._FieldAccessExpr [
                  R._FieldAccessExpr_object>>: var "sarg",
                  R._FieldAccessExpr_field>>: var "fname"])),
      _Elimination_union>>: lambda "cs" $
        "tname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ Core.caseStatementTypeName (var "cs"))) $
        "caseFields" <~ Core.caseStatementCases (var "cs") $
        "defCase" <~ Core.caseStatementDefault (var "cs") $
        "arms" <<~ (Eithers.mapList
          (lambda "cf" $
            "cfname" <~ (Formatting.capitalize @@ Core.unName (Core.fieldName (var "cf"))) $
            "cfterm" <~ Core.fieldTerm (var "cf") $
            "armBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ (Core.termApplication (Core.application (var "cfterm") (Core.termVariable (wrap _Name (string "v")))))) $
              right (record R._MatchArm [
                R._MatchArm_pattern>>:
                  inject R._Pattern R._Pattern_tupleStruct $
                    record R._TupleStructPattern [
                      R._TupleStructPattern_path>>:
                        record R._ExprPath [
                          R._ExprPath_global>>: boolean False,
                          R._ExprPath_segments>>: list [
                            record R._PathSegment [
                              R._PathSegment_name>>: Strings.cat2 (Strings.cat2 (var "tname") (string "::")) (var "cfname"),
                              R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
                      R._TupleStructPattern_elements>>: list [
                        inject R._Pattern R._Pattern_identifier $
                          record R._IdentifierPattern [
                            R._IdentifierPattern_name>>: string "v",
                            R._IdentifierPattern_mutable>>: boolean False,
                            R._IdentifierPattern_atPattern>>: nothing]]],
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "armBody"]))
          (var "caseFields")) $
        -- Add default arm if present
        "allArms" <<~ (Maybes.cases (var "defCase")
          (right (var "arms"))
          (lambda "dt" $
            "defBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ (Core.termApplication (Core.application (var "dt") (Core.termVariable (wrap _Name (string "v")))))) $
            right (Lists.concat2 (var "arms") (list [
              record R._MatchArm [
                R._MatchArm_pattern>>: inject R._Pattern R._Pattern_wildcard unit,
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "defBody"]])))) $
        Maybes.cases (var "marg")
          -- Unapplied: |v| match v { ... }
          (right (rustClosure @@ list [string "v"] @@
            (inject R._Expression R._Expression_match $
              record R._MatchExpr [
                R._MatchExpr_scrutinee>>: rustExprPath @@ string "v",
                R._MatchExpr_arms>>: var "allArms"])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (inject R._Expression R._Expression_match $
                record R._MatchExpr [
                  R._MatchExpr_scrutinee>>: var "sarg",
                  R._MatchExpr_arms>>: var "allArms"])),
      _Elimination_wrap>>: lambda "name" $
        Maybes.cases (var "marg")
          -- Unapplied: |v| v.0
          (right (rustClosure @@ list [string "v"] @@
            (inject R._Expression R._Expression_tupleIndex $
              record R._TupleIndexExpr [
                R._TupleIndexExpr_tuple>>: rustExprPath @@ string "v",
                R._TupleIndexExpr_index>>: int32 0])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (inject R._Expression R._Expression_tupleIndex $
                record R._TupleIndexExpr [
                  R._TupleIndexExpr_tuple>>: var "sarg",
                  R._TupleIndexExpr_index>>: int32 0]))]

-- =============================================================================
-- Struct field encoding
-- =============================================================================

-- | Encode a Hydra record field as a Rust struct field
encodeStructField :: TTermDefinition (Context -> Graph -> FieldType -> Either (InContext Error) R.StructField)
encodeStructField = def "encodeStructField" $
  "cx" ~> "g" ~> lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
      right (record R._StructField [
        R._StructField_name>>: Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ var "fname"),
        R._StructField_type>>: var "sftyp",
        R._StructField_public>>: boolean True,
        R._StructField_doc>>: nothing])

-- =============================================================================
-- Enum variant encoding
-- =============================================================================

-- | Encode a Hydra union field as a Rust enum variant
encodeEnumVariant :: TTermDefinition (Context -> Graph -> FieldType -> Either (InContext Error) R.EnumVariant)
encodeEnumVariant = def "encodeEnumVariant" $
  "cx" ~> "g" ~> lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "dtyp" <~ (Strip.deannotateType @@ var "ftyp") $
    "isUnit" <~ (cases _Type (var "dtyp") (Just $ boolean False) [
      _Type_unit>>: constant $ boolean True,
      _Type_record>>: lambda "rt" $
        Lists.null (var "rt")]) $
    Logic.ifElse (var "isUnit")
      -- Unit variant
      (right (record R._EnumVariant [
        R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
        R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_unit unit,
        R._EnumVariant_doc>>: nothing]))
      -- Non-unit variant: check if it's a record (struct variant) or other (tuple variant)
      (cases _Type (var "dtyp") (Just $
          -- Default: tuple variant with single element
          "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
            right (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_tuple $ list [var "sftyp"],
              R._EnumVariant_doc>>: nothing]))
        [_Type_record>>: lambda "rt" $
          "sfields" <<~ (Eithers.mapList (encodeStructField @@ var "cx" @@ var "g") (var "rt")) $
            right (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_struct (var "sfields"),
              R._EnumVariant_doc>>: nothing])])

-- =============================================================================
-- Type definition encoding
-- =============================================================================

-- | Encode a Hydra type definition as a Rust item
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> TypeDefinition -> Either (InContext Error) R.ItemWithComments)
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.typeDefinitionName (var "tdef") $
    "typ" <~ (Core.typeSchemeType $ Packaging.typeDefinitionType (var "tdef")) $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    -- Filter free type variables to unqualified names only (type parameters, not type references)
    "freeVars" <~ (Lists.filter
      (lambda "v" $ Lists.null (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "v")))))
      (Sets.toList (Variables.freeVariablesInType @@ var "typ"))) $
    "generics" <~ (Lists.map (lambda "v" $
      record R._GenericParam [
        R._GenericParam_name>>: Formatting.capitalize @@ Core.unName (var "v"),
        R._GenericParam_bounds>>: list ([] :: [TTerm R.TypeParamBound])])
      (var "freeVars")) $
    "dtyp" <~ (Strip.deannotateType @@ var "typ") $
    "item" <<~ (cases _Type (var "dtyp") (Just $
        -- Fallback: type alias
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
          right (inject R._Item R._Item_typeAlias $
            record R._TypeAlias [
              R._TypeAlias_name>>: var "lname",
              R._TypeAlias_generics>>: var "generics",
              R._TypeAlias_type>>: var "styp",
              R._TypeAlias_public>>: boolean True,
              R._TypeAlias_doc>>: nothing]))
      [_Type_record>>: lambda "rt" $
        "sfields" <<~ (Eithers.mapList (encodeStructField @@ var "cx" @@ var "g") (var "rt")) $
          right (inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "lname",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_named (var "sfields"),
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing]),
      _Type_union>>: lambda "rt" $
        "variants" <<~ (Eithers.mapList (encodeEnumVariant @@ var "cx" @@ var "g") (var "rt")) $
          right (inject R._Item R._Item_enum $
            record R._EnumDef [
              R._EnumDef_name>>: var "lname",
              R._EnumDef_generics>>: var "generics",
              R._EnumDef_whereClause>>: nothing,
              R._EnumDef_variants>>: var "variants",
              R._EnumDef_derives>>: asTerm standardDerives,
              R._EnumDef_public>>: boolean True,
              R._EnumDef_doc>>: nothing]),
      _Type_wrap>>: lambda "wt" $
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "wt") $
          right (inject R._Item R._Item_struct $
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
    right (record R._ItemWithComments [
      R._ItemWithComments_doc>>: nothing,
      R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
      R._ItemWithComments_item>>: var "item"])

-- =============================================================================
-- Term definition encoding
-- =============================================================================

-- | Encode a Hydra term definition as a Rust function item
encodeTermDefinition :: TTermDefinition (Context -> Graph -> TermDefinition -> Either (InContext Error) R.ItemWithComments)
encodeTermDefinition = def "encodeTermDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.termDefinitionName (var "tdef") $
    "term" <~ Packaging.termDefinitionTerm (var "tdef") $
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Names.localNameOf @@ var "name")) $
    "typ" <~ Maybes.maybe
      (Core.typeVariable (wrap _Name (string "hydra.core.Unit")))
      (unaryFunction Core.typeSchemeType)
      (Packaging.termDefinitionType (var "tdef")) $
    "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "term") $
    "retType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
      right (record R._ItemWithComments [
        R._ItemWithComments_doc>>: nothing,
        R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
        R._ItemWithComments_item>>: inject R._Item R._Item_fn $
          record R._FnDef [
            R._FnDef_name>>: var "lname",
            R._FnDef_generics>>: list ([] :: [TTerm R.GenericParam]),
            R._FnDef_whereClause>>: nothing,
            R._FnDef_params>>: list ([] :: [TTerm R.FnParam]),
            R._FnDef_returnType>>: just (var "retType"),
            R._FnDef_body>>: record R._Block [
              R._Block_statements>>: list ([] :: [TTerm R.Statement]),
              R._Block_expression>>: just (var "body")],
            R._FnDef_public>>: boolean True,
            R._FnDef_async>>: boolean False,
            R._FnDef_const>>: boolean False,
            R._FnDef_unsafe>>: boolean False,
            R._FnDef_doc>>: nothing]])

-- =============================================================================
-- Module entry point
-- =============================================================================

-- | Convert a Hydra module to a map of file paths to Rust source code strings.
moduleToRust :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToRust = def "moduleToRust" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    "typeItems" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "termItems" <<~ (Eithers.mapList (encodeTermDefinition @@ var "cx" @@ var "g") (var "termDefs")) $
    "allItems" <~ Lists.concat2 (var "typeItems") (var "termItems") $
    "crate" <~ (record R._Crate [R._Crate_items>>: var "allItems"]) $
    "code" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (RustSerdeSource.crateToExpr @@ var "crate"))) $
    "filePath" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "rs") @@ (Packaging.moduleNamespace (var "mod"))) $
      right (Maps.singleton (var "filePath") (var "code"))
