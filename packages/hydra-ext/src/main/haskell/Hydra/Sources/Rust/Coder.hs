-- | Rust code generator in Hydra DSL.
-- This module provides DSL versions of Rust code generation functions.
-- Type definitions are mapped to structs/enums/newtypes; term definitions are mapped to functions.

module Hydra.Sources.Rust.Coder where

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
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Coders                          as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                          as Error
import qualified Hydra.Dsl.Packaging                       as Module
import qualified Hydra.Dsl.Util                            as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Analysis       as CoderUtils
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Rust AST
import qualified Hydra.Rust.Syntax as R
import qualified Hydra.Sources.Rust.Syntax as RustSyntax
import qualified Hydra.Sources.Rust.Serde as RustSerdeSource
import qualified Hydra.Sources.Rust.Language as RustLanguageSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.rust.coder"

module_ :: Module
module_ = Module ns definitions
    [moduleNamespace RustSerdeSource.module_, moduleNamespace RustLanguageSource.module_,
      Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns, Lexical.ns, SerializationSource.ns, CoderUtils.ns]
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
      toDefinition rustExprPathSegmented,
      toDefinition rustQualifiedExprPath,
      toDefinition rustCall,
      toDefinition rustClone,
      toDefinition rustBlock,
      toDefinition rustLetStmt,
      toDefinition rustClosure,
      toDefinition encodeLiteralType,
      toDefinition encodeLiteral,
      toDefinition encodeType,
      toDefinition encodeTerm,
      toDefinition encodeTermAsArg,
      toDefinition qualifiedRustTypeName,
      toDefinition qualifiedRustVariantName,
      toDefinition rcWrapperItem,
      toDefinition encodeRustLambda,
      toDefinition encodeRustProjection,
      toDefinition encodeRustCases,
      toDefinition encodeRustUnwrap,
      toDefinition encodeStructField,
      toDefinition encodeEnumVariant,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTermDefinition,
      toDefinition collectLambdaParams,
      toDefinition collectApplicationSpine,
      toDefinition peelLambdas,
      toDefinition analyzeRustFunction,
      toDefinition isFunctionType,
      toDefinition encodeRcWrappedLambda,
      toDefinition encodeParamType,
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

-- | Expression path with multiple segments (e.g., ["hydra", "lib", "strings", "to_upper"])
rustExprPathSegmented :: TTermDefinition ([String] -> R.Expression)
rustExprPathSegmented = def "rustExprPathSegmented" $
  lambda "segs" $
    inject R._Expression R._Expression_path $
      record R._ExprPath [
        R._ExprPath_global>>: boolean False,
        R._ExprPath_segments>>:
          Lists.map (lambda "s" $
            record R._PathSegment [
              R._PathSegment_name>>: var "s",
              R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit])
          (var "segs")]

-- | Convert a dotted qualified name to a Rust expression path with :: separators
-- Qualified names get "crate::" prefix for cross-module references
rustQualifiedExprPath :: TTermDefinition (String -> R.Expression)
rustQualifiedExprPath = def "rustQualifiedExprPath" $
  lambda "name" $
    "parts" <~ Strings.splitOn (string ".") (var "name") $
    Logic.ifElse (Lists.null (Lists.tail (var "parts")))
      -- Simple unqualified name
      (rustExprPath @@ (Formatting.convertCaseCamelToLowerSnake @@ var "name"))
      -- Qualified name: prepend "crate", convert namespace segments to snake_case, sanitize last segment
      (rustExprPathSegmented @@ Lists.cons (string "crate") (Lists.concat2
        (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Lists.init (var "parts")))
        (list [Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Lists.last (var "parts"))])))

-- | Function call expression
rustCall :: TTermDefinition (R.Expression -> [R.Expression] -> R.Expression)
rustCall = def "rustCall" $
  lambda "fun" $ lambda "args" $
    inject R._Expression R._Expression_call $
      record R._CallExpr [
        R._CallExpr_function>>: var "fun",
        R._CallExpr_args>>: var "args"]

-- | Method call .clone() on an expression
rustClone :: TTermDefinition (R.Expression -> R.Expression)
rustClone = def "rustClone" $
  lambda "expr" $
    inject R._Expression R._Expression_methodCall $
      record R._MethodCallExpr [
        R._MethodCallExpr_receiver>>: var "expr",
        R._MethodCallExpr_method>>: string "clone",
        R._MethodCallExpr_turbofish>>: list ([] :: [TTerm R.Type]),
        R._MethodCallExpr_args>>: list ([] :: [TTerm R.Expression])]

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
-- Note: this generates a bare closure. When the closure is used as a value
-- (stored in a field or returned), the caller wraps it in Rc::new().
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
        _FloatType_bigfloat>>: constant $ rustApply1 @@ string "OrderedFloat" @@ (rustPath @@ string "f64"),
        _FloatType_float32>>: constant $ rustApply1 @@ string "OrderedFloat" @@ (rustPath @@ string "f32"),
        _FloatType_float64>>: constant $ rustApply1 @@ string "OrderedFloat" @@ (rustPath @@ string "f64")],
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
      rustCall @@ (rustExprPath @@ string "String::from") @@ list [
        inject R._Expression R._Expression_literal $
          inject R._Literal R._Literal_string (var "s")],
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
    "typ" <~ (Rewriting.deannotateType @@ var "t") $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
      _Type_application>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at"),
      _Type_unit>>: constant $
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
          right (rustApply1 @@ string "Rc"
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
        Logic.ifElse (Lists.null (var "rt"))
          (right (asTerm rustUnit))
          ("fieldTypes" <<~ (Eithers.mapList (lambda "ft" $
            encodeType @@ var "cx" @@ var "g" @@ Core.fieldTypeType (var "ft")) (var "rt")) $
            right (inject R._Type R._Type_tuple (var "fieldTypes"))),
      _Type_union>>: lambda "ut" $
        Logic.ifElse (Lists.null (var "ut"))
          (right (asTerm rustUnit))
          -- Non-empty anonymous union: map to a tuple of variant types as a fallback
          ("variantTypes" <<~ (Eithers.mapList (lambda "ft" $
            encodeType @@ var "cx" @@ var "g" @@ Core.fieldTypeType (var "ft")) (var "ut")) $
            right (inject R._Type R._Type_tuple (var "variantTypes"))),
      _Type_wrap>>: lambda "wt" $
        encodeType @@ var "cx" @@ var "g" @@ var "wt",
      _Type_variable>>: lambda "name" $
        "rawName" <~ Core.unName (var "name") $
        "parts" <~ Strings.splitOn (string ".") (var "rawName") $
        Logic.ifElse (Lists.null (Lists.tail (var "parts")))
          -- Simple unqualified name (type parameter): capitalize
          (right (rustPath @@ (Formatting.capitalize @@ var "rawName")))
          -- Qualified name (e.g., hydra.core.Term): convert to crate::hydra::core::Term
          ("segs" <~ Lists.cons (string "crate") (Lists.concat2
            (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Lists.init (var "parts")))
            (list [Formatting.capitalize @@ Lists.last (var "parts")])) $
          right (rustPathSegmented @@ var "segs")),
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
       -- Collect the full application spine with type applications (shared with Java/Python)
       "gathered" <~ (CoderUtils.gatherArgsWithTypeApps @@ var "term" @@ list ([] :: [TTerm Term]) @@ list ([] :: [TTerm Type])) $
       "headTerm" <~ Pairs.first (var "gathered") $
       "argTerms" <~ Pairs.first (Pairs.second (var "gathered")) $
       "dHead" <~ (Rewriting.deannotateTerm @@ var "headTerm") $
       -- Check if the head term is an elimination (case statement applied to an argument)
       cases _Term (var "dHead") (Just $
         -- Default: encode as function call
         "sfun" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "headTerm") $
         "sargs" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "argTerms")) $
           right (rustCall @@ var "sfun" @@ var "sargs"))
       -- Dispatch on the spine head: project/cases/unwrap eliminations get their
       -- first argument fed directly into the dedicated encoder; variable references
       -- (which post-#332 are how primitives are named) get a primitive-style call.
       [_Term_project>>: lambda "proj" $
          Logic.ifElse (Lists.null (var "argTerms"))
            (encodeRustProjection @@ var "cx" @@ var "g" @@ var "proj" @@ nothing)
            (encodeRustProjection @@ var "cx" @@ var "g" @@ var "proj" @@ just (Lists.head (var "argTerms"))),
        _Term_cases>>: lambda "cs" $
          Logic.ifElse (Lists.null (var "argTerms"))
            (encodeRustCases @@ var "cx" @@ var "g" @@ var "cs" @@ nothing)
            (encodeRustCases @@ var "cx" @@ var "g" @@ var "cs" @@ just (Lists.head (var "argTerms"))),
        _Term_unwrap>>: lambda "name" $
          Logic.ifElse (Lists.null (var "argTerms"))
            (encodeRustUnwrap @@ var "cx" @@ var "g" @@ var "name" @@ nothing)
            (encodeRustUnwrap @@ var "cx" @@ var "g" @@ var "name" @@ just (Lists.head (var "argTerms"))),
        _Term_variable>>: lambda "name" $
          -- Primitive (or user-defined function) call with all arguments
          "sfun" <~ (rustQualifiedExprPath @@ Core.unName (var "name")) $
          "sargs" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "argTerms")) $
            right (rustCall @@ var "sfun" @@ var "sargs")],
     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $
           "sl" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "l") $
             right (rustCall @@ (rustExprPath @@ string "Left") @@ list [var "sl"]))
         (lambda "r" $
           "sr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "r") $
             right (rustCall @@ (rustExprPath @@ string "Right") @@ list [var "sr"]))
         (var "e"),
     _Term_lambda>>: lambda "lam" $
       encodeRustLambda @@ var "cx" @@ var "g" @@ var "lam",
     _Term_project>>: lambda "proj" $
       encodeRustProjection @@ var "cx" @@ var "g" @@ var "proj" @@ nothing,
     _Term_cases>>: lambda "cs" $
       encodeRustCases @@ var "cx" @@ var "g" @@ var "cs" @@ nothing,
     _Term_unwrap>>: lambda "name" $
       encodeRustUnwrap @@ var "cx" @@ var "g" @@ var "name" @@ nothing,
     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       "stmts" <<~ (Eithers.mapList
         (lambda "b" $
           "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (Core.bindingName (var "b")))) $
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
       -- Look up record field types from schema for type-directed Rc wrapping
       "mRecordType" <~ Eithers.either_ (constant nothing) (lambda "t" $ just (Rewriting.deannotateType @@ var "t"))
         (Schemas.requireType @@ var "cx" @@ var "g" @@ var "rname") $
       "sfields" <<~ (Eithers.mapList
         (lambda "f" $
           "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (Core.fieldName (var "f")))) $
           -- Look up this field's type to detect function-typed fields
           "mFieldType" <~ Maybes.bind (var "mRecordType") (lambda "rt" $
             cases _Type (var "rt") (Just nothing)
               [_Type_record>>: lambda "fts" $
                 "matching" <~ Lists.filter (lambda "ft" $
                   Equality.equal (Core.unName (Core.fieldTypeName (var "ft"))) (Core.unName (Core.fieldName (var "f")))) (var "fts") $
                 Logic.ifElse (Lists.null (var "matching"))
                   nothing
                   (just (Core.fieldTypeType (Lists.head (var "matching"))))]) $
           -- Check if field type is a function type AND field value is a lambda
           "dFieldTerm" <~ (Rewriting.deannotateTerm @@ Core.fieldTerm (var "f")) $
           "isLambdaField" <~ (cases _Term (var "dFieldTerm") (Just $ boolean False)
             [_Term_lambda>>: constant $ boolean True]) $
           "needsRcWrap" <~ (Logic.and (var "isLambdaField")
             (Maybes.maybe (boolean False) isFunctionType (var "mFieldType"))) $
           -- Encode field value: Rc-wrap if function-typed, else normal
           "fval" <<~ (Logic.ifElse (var "needsRcWrap")
             (encodeRcWrappedLambda @@ var "cx" @@ var "g" @@ var "dFieldTerm")
             (encodeTerm @@ var "cx" @@ var "g" @@ Core.fieldTerm (var "f"))) $
             right (record R._FieldValue [
               R._FieldValue_name>>: var "fname",
               R._FieldValue_value>>: just (var "fval")]))
         (var "fields")) $
       -- Build TypeName_Variant { fields... }
       "innerExpr" <~ (inject R._Expression R._Expression_struct $
         record R._StructExpr [
           R._StructExpr_path>>:
             record R._ExprPath [
               R._ExprPath_global>>: boolean False,
               R._ExprPath_segments>>: list [
                 record R._PathSegment [
                   R._PathSegment_name>>: qualifiedRustVariantName @@ var "rname",
                   R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
           R._StructExpr_fields>>: var "sfields",
           R._StructExpr_rest>>: nothing]) $
       -- Wrap in TypeName(Rc::new(...))
       "rcExpr" <~ (rustCall @@ (rustExprPathSegmented @@ list [string "Rc", string "new"]) @@ list [var "innerExpr"]) $
         right (rustCall @@ (rustExprPath @@ (qualifiedRustTypeName @@ var "rname")) @@ list [var "rcExpr"]),
     _Term_set>>: lambda "s" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (Sets.toList (var "s"))) $
         right (rustCall @@ (rustExprPath @@ string "BTreeSet::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "sels")]),
     _Term_union>>: lambda "inj" $
       "tname" <~ (qualifiedRustTypeName @@ Core.injectionTypeName (var "inj")) $
       "vname" <~ (qualifiedRustVariantName @@ Core.injectionTypeName (var "inj")) $
       "field" <~ Core.injectionField (var "inj") $
       "fname" <~ (Formatting.capitalize @@ Core.unName (Core.fieldName (var "field"))) $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Rewriting.deannotateTerm @@ var "fterm") $
       -- Check if the variant is unit by looking up the type definition from the schema
       "fieldName" <~ Core.fieldName (var "field") $
       "isUnitFromTerm" <~ (cases _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.null (Core.recordFields (var "rt"))]) $
       -- Also check via the schema: look up the union type's field types
       "isUnitFromSchema" <~ Eithers.either_ (constant $ boolean False) (lambda "unionType" $
         "dunionType" <~ (Rewriting.deannotateType @@ var "unionType") $
         cases _Type (var "dunionType") (Just $ boolean False)
           [_Type_union>>: lambda "fts" $
             "matchingFields" <~ Lists.filter (lambda "ft" $
               Equality.equal (Core.unName (Core.fieldTypeName (var "ft"))) (Core.unName (var "fieldName"))) (var "fts") $
             Logic.ifElse (Lists.null (var "matchingFields")) (boolean False)
               ("matchType" <~ (Rewriting.deannotateType @@ Core.fieldTypeType (Lists.head (var "matchingFields"))) $
                cases _Type (var "matchType") (Just $ boolean False)
                  [_Type_unit>>: constant $ boolean True,
                   _Type_record>>: lambda "rt" $ Lists.null (var "rt")])])
         (Schemas.requireType @@ var "cx" @@ var "g" @@ Core.injectionTypeName (var "inj")) $
       "isUnit" <~ (Logic.ifElse (var "isUnitFromTerm") (boolean True) (var "isUnitFromSchema")) $
       -- Build the inner variant expression: TypeName_Variant::Foo or TypeName_Variant::Foo(val)
       "variantPath" <~ Strings.cat2 (Strings.cat2 (var "vname") (string "::")) (var "fname") $
       "innerExpr" <<~ Logic.ifElse (var "isUnit")
         (right (rustExprPath @@ var "variantPath"))
         ("sval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fterm") $
           right (rustCall @@ (rustExprPath @@ var "variantPath") @@ list [var "sval"])) $
       -- Wrap in TypeName(Rc::new(...))
       "rcExpr" <~ (rustCall @@ (rustExprPathSegmented @@ list [string "Rc", string "new"]) @@ list [var "innerExpr"]) $
         right (rustCall @@ (rustExprPath @@ var "tname") @@ list [var "rcExpr"]),
     _Term_unit>>: constant $
       right (inject R._Expression R._Expression_tuple $ list ([] :: [TTerm R.Expression])),
     _Term_variable>>: lambda "name" $
       "rawName" <~ Core.unName (var "name") $
       "parts" <~ Strings.splitOn (string ".") (var "rawName") $
       Logic.ifElse (Lists.null (Lists.tail (var "parts")))
         -- Unqualified name: sanitize, convert to snake_case, clone for Rust move semantics
         (right (rustClone @@ (rustExprPath @@ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ var "rawName")))))
         -- Qualified name: prepend crate::, convert namespace parts to snake_case, sanitize local part
         ("segs" <~ Lists.cons (string "crate") (Lists.concat2
           (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Lists.init (var "parts")))
           (list [Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Lists.last (var "parts"))])) $
         right (rustExprPathSegmented @@ var "segs")),
     _Term_wrap>>: lambda "wt" $
       "tname" <~ (qualifiedRustTypeName @@ Core.wrappedTermTypeName (var "wt")) $
       "vname" <~ (qualifiedRustVariantName @@ Core.wrappedTermTypeName (var "wt")) $
       "inner" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.wrappedTermBody (var "wt")) $
       -- Build TypeName_Variant(inner), then wrap in Rc::new, then TypeName(...)
       "variantExpr" <~ (rustCall @@ (rustExprPath @@ var "vname") @@ list [var "inner"]) $
       "rcExpr" <~ (rustCall @@ (rustExprPathSegmented @@ list [string "Rc", string "new"]) @@ list [var "variantExpr"]) $
         right (rustCall @@ (rustExprPath @@ var "tname") @@ list [var "rcExpr"]),
     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.typeApplicationTermBody (var "ta"),
     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.typeLambdaBody (var "tl")]

-- | Convert a Hydra type name (e.g., "hydra.core.Term") to a qualified Rust type path
-- (e.g., "crate::hydra::core::Term"). For unqualified names, returns just the capitalized name.
qualifiedRustTypeName :: TTermDefinition (Name -> String)
qualifiedRustTypeName = def "qualifiedRustTypeName" $
  lambda "name" $
    "rawName" <~ Core.unName (var "name") $
    "parts" <~ Strings.splitOn (string ".") (var "rawName") $
    Logic.ifElse (Lists.null (Lists.tail (var "parts")))
      (Formatting.capitalize @@ var "rawName")
      (Strings.intercalate (string "::") (Lists.concat2
        (list [string "crate"])
        (Lists.concat2
          (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Lists.init (var "parts")))
          (list [Formatting.capitalize @@ Lists.last (var "parts")]))))

-- | Like qualifiedRustTypeName, but appends "_Variant" to the final type name.
-- Used when constructing or matching on the inner enum/struct of an Rc-wrapped type.
qualifiedRustVariantName :: TTermDefinition (Name -> String)
qualifiedRustVariantName = def "qualifiedRustVariantName" $
  lambda "name" $
    "rawName" <~ Core.unName (var "name") $
    "parts" <~ Strings.splitOn (string ".") (var "rawName") $
    Logic.ifElse (Lists.null (Lists.tail (var "parts")))
      (Strings.cat2 (Formatting.capitalize @@ var "rawName") (string "_Variant"))
      (Strings.intercalate (string "::") (Lists.concat2
        (list [string "crate"])
        (Lists.concat2
          (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Lists.init (var "parts")))
          (list [Strings.cat2 (Formatting.capitalize @@ Lists.last (var "parts")) (string "_Variant")]))))

-- | Generate the Rc wrapper struct for a type definition.
-- Given a type name "Foo" and generics, produces:
--   pub struct Foo<T>(pub Rc<Foo_Variant<T>>);
rcWrapperItem :: TTermDefinition (String -> [R.GenericParam] -> R.ItemWithComments)
rcWrapperItem = def "rcWrapperItem" $
  lambda "lname" $ lambda "generics" $
    "variantName" <~ Strings.cat2 (var "lname") (string "_Variant") $
    -- Build the Rc<Foo_Variant<T>> type: apply generic params if any
    "innerType" <~ Logic.ifElse (Lists.null (var "generics"))
      (rustPath @@ var "variantName")
      (inject R._Type R._Type_path $
        record R._TypePath [
          R._TypePath_global>>: boolean False,
          R._TypePath_segments>>: list [
            record R._PathSegment [
              R._PathSegment_name>>: var "variantName",
              R._PathSegment_arguments>>:
                inject R._GenericArguments R._GenericArguments_angleBracketed $
                  record R._AngleBracketedArgs [
                    R._AngleBracketedArgs_args>>:
                      Lists.map (lambda "gp" $
                        inject R._GenericArg R._GenericArg_type
                          (rustPath @@ (project R._GenericParam R._GenericParam_name @@ var "gp")))
                        (var "generics")]]]]) $
    "rcType" <~ (rustApply1 @@ string "Rc" @@ var "innerType") $
    record R._ItemWithComments [
      R._ItemWithComments_doc>>: nothing,
      R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
      R._ItemWithComments_item>>: inject R._Item R._Item_struct $
        record R._StructDef [
          R._StructDef_name>>: var "lname",
          R._StructDef_generics>>: var "generics",
          R._StructDef_whereClause>>: nothing,
          R._StructDef_body>>: inject R._StructBody R._StructBody_tuple $ list [
            record R._TupleField [
              R._TupleField_type>>: var "rcType",
              R._TupleField_public>>: boolean True]],
          R._StructDef_derives>>: asTerm standardDerives,
          R._StructDef_public>>: boolean True,
          R._StructDef_doc>>: nothing]]

-- | Encode a term that will be used as a function argument.
-- When the term is a function reference (primitive/variable pointing to a function),
-- wrap it in Rc::new() to convert fn items to Rc<dyn Fn>.
encodeTermAsArg :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) R.Expression)
encodeTermAsArg = def "encodeTermAsArg" $
  "cx" ~> "g" ~> lambda "term" $
    "dterm" <~ (Rewriting.deannotateTerm @@ var "term") $
    cases _Term (var "dterm") (Just $
      encodeTerm @@ var "cx" @@ var "g" @@ var "term")
    [_Term_variable>>: lambda "name" $
      -- Variable/primitive reference used as argument: wrap in Rc::new() for Rc<dyn Fn> compatibility
      "sfun" <~ (rustQualifiedExprPath @@ Core.unName (var "name")) $
        right (rustCall @@ (rustExprPathSegmented @@ list [string "Rc", string "new"]) @@ list [var "sfun"])]

-- =============================================================================
-- Lambda encoding
-- =============================================================================

-- | Encode a Hydra lambda as a Rust closure expression.
-- Post-#332: lambdas are a direct variant of Term (no longer wrapped in Function).
encodeRustLambda :: TTermDefinition (Context -> Graph -> Lambda -> Either (InContext Error) R.Expression)
encodeRustLambda = def "encodeRustLambda" $
  "cx" ~> "g" ~> lambda "lam" $
        -- Collect nested lambdas into a single multi-param closure with type annotations
        "collected" <~ (collectLambdaParams @@ Core.termLambda (var "lam")) $
        "rawParams" <~ Pairs.first (var "collected") $
        "closureParams" <~ (Lists.map (lambda "pt" $
          "pname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Pairs.first (var "pt"))) $
          "mtyp" <~ Pairs.second (var "pt") $
          record R._ClosureParam [
            R._ClosureParam_pattern>>:
              inject R._Pattern R._Pattern_identifier $
                record R._IdentifierPattern [
                  R._IdentifierPattern_name>>: var "pname",
                  R._IdentifierPattern_mutable>>: boolean False,
                  R._IdentifierPattern_atPattern>>: nothing],
            R._ClosureParam_type>>:
              Maybes.bind (var "mtyp") (lambda "t" $
                Eithers.either_ (constant nothing) (lambda "rt" $ just (var "rt"))
                  (encodeType @@ var "cx" @@ var "g" @@ var "t"))])
          (var "rawParams")) $
        "innerBody" <~ Pairs.second (var "collected") $
        "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "innerBody") $
          -- Note: bare closure, NOT Rc-wrapped. Most closures are passed to impl Fn
          -- parameters. Rc wrapping would break those calls. Closures used as values
          -- (stored in struct fields) will need Rc wrapping at the call site instead.
          right (inject R._Expression R._Expression_closure $
            record R._ClosureExpr [
              R._ClosureExpr_move>>: boolean False,
              R._ClosureExpr_params>>: var "closureParams",
              R._ClosureExpr_returnType>>: nothing,
              R._ClosureExpr_body>>: var "body"])

-- =============================================================================
-- Elimination encoding (post-#332: split into one function per elim variant)
-- =============================================================================

-- | Encode a Hydra record projection as a Rust expression.
-- Takes an optional argument for applied projections.
encodeRustProjection :: TTermDefinition (Context -> Graph -> Projection -> Maybe Term -> Either (InContext Error) R.Expression)
encodeRustProjection = def "encodeRustProjection" $
  "cx" ~> "g" ~> lambda "proj" $ lambda "marg" $
        "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (Core.projectionField (var "proj")))) $
        Maybes.cases (var "marg")
          -- Unapplied projection: |v| v.0.field.clone()
          (right (rustClosure @@ list [string "v"] @@
            (rustClone @@ (inject R._Expression R._Expression_fieldAccess $
              record R._FieldAccessExpr [
                R._FieldAccessExpr_object>>:
                  inject R._Expression R._Expression_tupleIndex $
                    record R._TupleIndexExpr [
                      R._TupleIndexExpr_tuple>>: rustExprPath @@ string "v",
                      R._TupleIndexExpr_index>>: int32 0],
                R._FieldAccessExpr_field>>: var "fname"]))))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (rustClone @@ (inject R._Expression R._Expression_fieldAccess $
                record R._FieldAccessExpr [
                  R._FieldAccessExpr_object>>:
                    inject R._Expression R._Expression_tupleIndex $
                      record R._TupleIndexExpr [
                        R._TupleIndexExpr_tuple>>: var "sarg",
                        R._TupleIndexExpr_index>>: int32 0],
                  R._FieldAccessExpr_field>>: var "fname"])))

-- | Encode a Hydra case statement as a Rust match expression.
-- Takes an optional argument for applied case statements.
encodeRustCases :: TTermDefinition (Context -> Graph -> CaseStatement -> Maybe Term -> Either (InContext Error) R.Expression)
encodeRustCases = def "encodeRustCases" $
  "cx" ~> "g" ~> lambda "cs" $ lambda "marg" $
        "tname" <~ (qualifiedRustTypeName @@ Core.caseStatementTypeName (var "cs")) $
        "vname" <~ (qualifiedRustVariantName @@ Core.caseStatementTypeName (var "cs")) $
        "caseFields" <~ Core.caseStatementCases (var "cs") $
        "defCase" <~ Core.caseStatementDefault (var "cs") $
        "arms" <<~ (Eithers.mapList
          (lambda "cf" $
            "cfname" <~ (Formatting.capitalize @@ Core.unName (Core.fieldName (var "cf"))) $
            "cfterm" <~ Core.fieldTerm (var "cf") $
            -- Peel one lambda from the case function to get the body with 'v' substituted
            "dcfterm" <~ (Rewriting.deannotateTerm @@ var "cfterm") $
            "armBody" <<~ (cases _Term (var "dcfterm") (Just $
                -- Non-lambda term (primitive/projection/etc.): apply to v0_
                "sfun" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "cfterm") $
                  right (rustCall @@ var "sfun" @@ list [rustExprPath @@ string "v0_"]))
              [_Term_lambda>>: lambda "lam" $
                -- Lambda: encode the body with the lambda param replaced by 'v0_'
                "lamBody" <~ Core.lambdaBody (var "lam") $
                encodeTerm @@ var "cx" @@ var "g" @@ (Rewriting.substituteVariable @@ Core.lambdaParameter (var "lam") @@ wrap _Name (string "v0_") @@ var "lamBody")]) $
              -- Wrap arm body in { let v0_ = v0_.clone(); <body> } to own the matched reference
              "clonedBody" <~ (rustBlock
                @@ list [rustLetStmt @@ string "v0_" @@ (rustClone @@ (rustExprPath @@ string "v0_"))]
                @@ var "armBody") $
              right (record R._MatchArm [
                R._MatchArm_pattern>>:
                  inject R._Pattern R._Pattern_tupleStruct $
                    record R._TupleStructPattern [
                      R._TupleStructPattern_path>>:
                        record R._ExprPath [
                          R._ExprPath_global>>: boolean False,
                          R._ExprPath_segments>>: list [
                            record R._PathSegment [
                              -- Use _Variant name for pattern matching on the inner enum
                              R._PathSegment_name>>: Strings.cat2 (Strings.cat2 (var "vname") (string "::")) (var "cfname"),
                              R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
                      R._TupleStructPattern_elements>>: list [
                        inject R._Pattern R._Pattern_identifier $
                          record R._IdentifierPattern [
                            R._IdentifierPattern_name>>: string "v0_",
                            R._IdentifierPattern_mutable>>: boolean False,
                            R._IdentifierPattern_atPattern>>: nothing]]],
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "clonedBody"]))
          (var "caseFields")) $
        -- Add default arm if present
        "allArms" <<~ (Maybes.cases (var "defCase")
          (right (var "arms"))
          (lambda "dt" $
            "ddt" <~ (Rewriting.deannotateTerm @@ var "dt") $
            "defBody" <<~ (cases _Term (var "ddt") (Just $
                -- Non-lambda term: apply as a function to v0_
                "sfun" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "dt") $
                  right (rustCall @@ var "sfun" @@ list [rustExprPath @@ string "v0_"]))
              [_Term_lambda>>: lambda "lam" $
                "lamBody" <~ Core.lambdaBody (var "lam") $
                encodeTerm @@ var "cx" @@ var "g" @@ (Rewriting.substituteVariable @@ Core.lambdaParameter (var "lam") @@ wrap _Name (string "v0_") @@ var "lamBody")]) $
            right (Lists.concat2 (var "arms") (list [
              record R._MatchArm [
                R._MatchArm_pattern>>: inject R._Pattern R._Pattern_wildcard unit,
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "defBody"]])))) $
        Maybes.cases (var "marg")
          -- Unapplied: |v| match &*v.0 { ... }
          ("scrutinee" <~ (inject R._Expression R._Expression_reference $
            record R._RefExpr [
              R._RefExpr_mutable>>: boolean False,
              R._RefExpr_expr>>: inject R._Expression R._Expression_dereference $
                inject R._Expression R._Expression_tupleIndex $
                  record R._TupleIndexExpr [
                    R._TupleIndexExpr_tuple>>: rustExprPath @@ string "v",
                    R._TupleIndexExpr_index>>: int32 0]]) $
          right (rustClosure @@ list [string "v"] @@
            (inject R._Expression R._Expression_match $
              record R._MatchExpr [
                R._MatchExpr_scrutinee>>: var "scrutinee",
                R._MatchExpr_arms>>: var "allArms"])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
            -- match &*sarg.0 { ... }
            "scrutinee" <~ (inject R._Expression R._Expression_reference $
              record R._RefExpr [
                R._RefExpr_mutable>>: boolean False,
                R._RefExpr_expr>>: inject R._Expression R._Expression_dereference $
                  inject R._Expression R._Expression_tupleIndex $
                    record R._TupleIndexExpr [
                      R._TupleIndexExpr_tuple>>: var "sarg",
                      R._TupleIndexExpr_index>>: int32 0]]) $
              right (inject R._Expression R._Expression_match $
                record R._MatchExpr [
                  R._MatchExpr_scrutinee>>: var "scrutinee",
                  R._MatchExpr_arms>>: var "allArms"]))

-- | Encode a Hydra unwrap (newtype elimination) as a Rust expression.
-- Takes an optional argument for applied unwraps.
encodeRustUnwrap :: TTermDefinition (Context -> Graph -> Name -> Maybe Term -> Either (InContext Error) R.Expression)
encodeRustUnwrap = def "encodeRustUnwrap" $
  "cx" ~> "g" ~> lambda "name" $ lambda "marg" $
        Maybes.cases (var "marg")
          -- Unapplied: |v| v.0.0.clone()
          (right (rustClosure @@ list [string "v"] @@
            (rustClone @@ (inject R._Expression R._Expression_tupleIndex $
              record R._TupleIndexExpr [
                R._TupleIndexExpr_tuple>>:
                  inject R._Expression R._Expression_tupleIndex $
                    record R._TupleIndexExpr [
                      R._TupleIndexExpr_tuple>>: rustExprPath @@ string "v",
                      R._TupleIndexExpr_index>>: int32 0],
                R._TupleIndexExpr_index>>: int32 0]))))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (rustClone @@ (inject R._Expression R._Expression_tupleIndex $
                record R._TupleIndexExpr [
                  R._TupleIndexExpr_tuple>>:
                    inject R._Expression R._Expression_tupleIndex $
                      record R._TupleIndexExpr [
                        R._TupleIndexExpr_tuple>>: var "sarg",
                        R._TupleIndexExpr_index>>: int32 0],
                  R._TupleIndexExpr_index>>: int32 0])))

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
    "dtyp" <~ (Rewriting.deannotateType @@ var "ftyp") $
    "isUnit" <~ (cases _Type (var "dtyp") (Just $ boolean False) [
      _Type_unit>>: constant $ boolean True,
      _Type_record>>: lambda "rt" $
        Lists.null (var "rt")]) $
    -- Note: no Box wrapping — recursive types need separate handling
    Logic.ifElse (var "isUnit")
      -- Unit variant
      (right (record R._EnumVariant [
        R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
        R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_unit unit,
        R._EnumVariant_doc>>: nothing]))
      -- Non-unit variant
      (cases _Type (var "dtyp") (Just $
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

-- | Encode a Hydra type definition as a list of Rust items.
-- Named types (records, unions, newtypes) produce two items:
--   1. The inner type with "_Variant" suffix (the actual struct/enum/tuple-struct)
--   2. An Rc wrapper: pub struct Foo(pub Rc<Foo_Variant>);
-- Type aliases produce a single item (no Rc wrapping).
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> TypeDefinition -> Either (InContext Error) [R.ItemWithComments])
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Module.typeDefinitionName (var "tdef") $
    "typ" <~ Module.typeDefinitionType (var "tdef") $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    "variantName" <~ Strings.cat2 (var "lname") (string "_Variant") $
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
    cases _Type (var "dtyp") (Just $
        -- Fallback: type alias (no Rc wrapping, single item)
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
          right (list [record R._ItemWithComments [
            R._ItemWithComments_doc>>: nothing,
            R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
            R._ItemWithComments_item>>: inject R._Item R._Item_typeAlias $
              record R._TypeAlias [
                R._TypeAlias_name>>: var "lname",
                R._TypeAlias_generics>>: var "generics",
                R._TypeAlias_type>>: var "styp",
                R._TypeAlias_public>>: boolean True,
                R._TypeAlias_doc>>: nothing]]]))
      [_Type_record>>: lambda "rt" $
        "sfields" <<~ (Eithers.mapList (encodeStructField @@ var "cx" @@ var "g") (var "rt")) $
        "innerItem" <~ (record R._ItemWithComments [
          R._ItemWithComments_doc>>: nothing,
          R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
          R._ItemWithComments_item>>: inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "variantName",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_named (var "sfields"),
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing]]) $
          right (list [var "innerItem", rcWrapperItem @@ var "lname" @@ var "generics"]),
      _Type_union>>: lambda "rt" $
        "variants" <<~ (Eithers.mapList (encodeEnumVariant @@ var "cx" @@ var "g") (var "rt")) $
        "innerItem" <~ (record R._ItemWithComments [
          R._ItemWithComments_doc>>: nothing,
          R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
          R._ItemWithComments_item>>: inject R._Item R._Item_enum $
            record R._EnumDef [
              R._EnumDef_name>>: var "variantName",
              R._EnumDef_generics>>: var "generics",
              R._EnumDef_whereClause>>: nothing,
              R._EnumDef_variants>>: var "variants",
              R._EnumDef_derives>>: asTerm standardDerives,
              R._EnumDef_public>>: boolean True,
              R._EnumDef_doc>>: nothing]]) $
          right (list [var "innerItem", rcWrapperItem @@ var "lname" @@ var "generics"]),
      _Type_wrap>>: lambda "wt" $
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "wt") $
        "innerItem" <~ (record R._ItemWithComments [
          R._ItemWithComments_doc>>: nothing,
          R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
          R._ItemWithComments_item>>: inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "variantName",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_tuple $ list [
                record R._TupleField [
                  R._TupleField_type>>: var "styp",
                  R._TupleField_public>>: boolean True]],
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing]]) $
          right (list [var "innerItem", rcWrapperItem @@ var "lname" @@ var "generics"])]

-- =============================================================================
-- Term definition encoding
-- =============================================================================

-- | Encode a Hydra term definition as a Rust function item.
-- Uses CoderUtils.analyzeFunctionTerm for proper function analysis.
encodeTermDefinition :: TTermDefinition (Context -> Graph -> TermDefinition -> Either (InContext Error) R.ItemWithComments)
encodeTermDefinition = def "encodeTermDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Module.termDefinitionName (var "tdef") $
    "term" <~ Module.termDefinitionTerm (var "tdef") $
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ (Names.localNameOf @@ var "name"))) $
    -- Analyze function structure using shared CoderUtils (same as Java/Python)
    "fs" <<~ (analyzeRustFunction @@ var "term" @@ var "cx" @@ var "g") $
    "params" <~ (project _FunctionStructure _FunctionStructure_params @@ var "fs") $
    "domains" <~ (project _FunctionStructure _FunctionStructure_domains @@ var "fs") $
    "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
    "innerBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
    "mCodomain" <~ (project _FunctionStructure _FunctionStructure_codomain @@ var "fs") $
    -- Encode function parameters (zip names with domain types)
    -- Use encodeParamType for parameters: generates impl Fn instead of Rc<dyn Fn>
    "fnParams" <<~ (Eithers.mapList
      (lambda "pd" $
        "pname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (Pairs.first (var "pd")))) $
        "ptyp" <<~ (encodeParamType @@ var "cx" @@ var "g" @@ Pairs.second (var "pd")) $
          right (record R._FnParam [
            R._FnParam_pattern>>:
              inject R._Pattern R._Pattern_identifier $
                record R._IdentifierPattern [
                  R._IdentifierPattern_name>>: var "pname",
                  R._IdentifierPattern_mutable>>: boolean False,
                  R._IdentifierPattern_atPattern>>: nothing],
            R._FnParam_type>>: var "ptyp"]))
      (Lists.zip (var "params") (var "domains"))) $
    -- Encode let bindings as statements
    "letStmts" <<~ (Eithers.mapList
      (lambda "b" $
        "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (Core.bindingName (var "b")))) $
        "bval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
          right (rustLetStmt @@ var "bname" @@ var "bval"))
      (var "bindings")) $
    -- Encode body and return type
    "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "innerBody") $
    "retType" <<~ (Maybes.maybe
      (right (asTerm rustUnit))
      (lambda "cod" $ encodeType @@ var "cx" @@ var "g" @@ var "cod")
      (var "mCodomain")) $
      right (record R._ItemWithComments [
        R._ItemWithComments_doc>>: nothing,
        R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
        R._ItemWithComments_item>>: inject R._Item R._Item_fn $
          record R._FnDef [
            R._FnDef_name>>: var "lname",
            R._FnDef_generics>>: list ([] :: [TTerm R.GenericParam]),
            R._FnDef_whereClause>>: nothing,
            R._FnDef_params>>: var "fnParams",
            R._FnDef_returnType>>: just (var "retType"),
            R._FnDef_body>>: record R._Block [
              R._Block_statements>>: var "letStmts",
              R._Block_expression>>: just (var "body")],
            R._FnDef_public>>: boolean True,
            R._FnDef_async>>: boolean False,
            R._FnDef_const>>: boolean False,
            R._FnDef_unsafe>>: boolean False,
            R._FnDef_doc>>: nothing]])

-- =============================================================================
-- Lambda param collection (multi-param closures)
-- =============================================================================

-- | Collect nested lambda parameters with optional domain types:
-- |a| |b| |c| body -> ([(a, Maybe Type), (b, Maybe Type), (c, Maybe Type)], body)
collectLambdaParams :: TTermDefinition (Term -> ([(String, Maybe Type)], Term))
collectLambdaParams = def "collectLambdaParams" $
  lambda "term" $
    "dterm" <~ (Rewriting.deannotateTerm @@ var "term") $
    cases _Term (var "dterm") (Just $ pair (list ([] :: [TTerm (String, Maybe Type)])) (var "term"))
    [_Term_lambda>>: lambda "lam" $
      "paramName" <~ Core.unName (Core.lambdaParameter (var "lam")) $
      "paramDomain" <~ Core.lambdaDomain (var "lam") $
      "body" <~ Core.lambdaBody (var "lam") $
      "rest" <~ (collectLambdaParams @@ var "body") $
      "restParams" <~ Pairs.first (var "rest") $
      "restBody" <~ Pairs.second (var "rest") $
        pair (Lists.cons (pair (var "paramName") (var "paramDomain")) (var "restParams")) (var "restBody")]

-- =============================================================================
-- Application spine collection (uncurried calls)
-- =============================================================================

-- | Collect the application spine: f(a)(b)(c) -> (f, [a, b, c])
collectApplicationSpine :: TTermDefinition (Term -> (Term, [Term]))
collectApplicationSpine = def "collectApplicationSpine" $
  lambda "term" $
    "dterm" <~ (Rewriting.deannotateTerm @@ var "term") $
    cases _Term (var "dterm") (Just $ pair (var "term") (list ([] :: [TTerm Term])))
    [_Term_application>>: lambda "app" $
      "funTerm" <~ Core.applicationFunction (var "app") $
      "argTerm" <~ Core.applicationArgument (var "app") $
      "inner" <~ (collectApplicationSpine @@ var "funTerm") $
      "headTerm" <~ Pairs.first (var "inner") $
      "prevArgs" <~ Pairs.second (var "inner") $
        pair (var "headTerm") (Lists.concat2 (var "prevArgs") (list [var "argTerm"]))]

-- =============================================================================
-- Lambda peeling (uncurrying)
-- =============================================================================

-- | Peel matching lambda/function-type pairs from a term and its type.
-- Returns ([(paramName, paramType)], (innerTerm, returnType))
peelLambdas :: TTermDefinition (Term -> Type -> ([(String, Type)], (Term, Type)))
peelLambdas = def "peelLambdas" $
  lambda "term" $ lambda "typ" $
    "dterm" <~ (Rewriting.deannotateTerm @@ var "term") $
    "dtyp" <~ (Rewriting.deannotateType @@ var "typ") $
    cases _Term (var "dterm") (Just $
      -- Not a lambda: return empty params, full term and type
      pair (list ([] :: [TTerm (String, Type)])) (pair (var "term") (var "typ")))
    [_Term_typeLambda>>: lambda "tl" $
      -- Skip through type lambda to the body
      peelLambdas @@ Core.typeLambdaBody (var "tl") @@ var "typ",
     _Term_lambda>>: lambda "lam" $
      "paramName" <~ Core.unName (Core.lambdaParameter (var "lam")) $
      "body" <~ Core.lambdaBody (var "lam") $
      -- Try to match the type as a function type (skip through forall)
      cases _Type (var "dtyp") (Just $
        -- Type doesn't match function type: use lambda's own domain if available
        Maybes.maybe
          (pair (list ([] :: [TTerm (String, Type)])) (pair (var "term") (var "typ")))
          (lambda "dom" $
            "rest" <~ (peelLambdas @@ var "body" @@ var "typ") $
            "restParams" <~ Pairs.first (var "rest") $
            "restInner" <~ Pairs.second (var "rest") $
              pair (Lists.cons (pair (var "paramName") (var "dom")) (var "restParams")) (var "restInner"))
          (Core.lambdaDomain (var "lam")))
      [_Type_function>>: lambda "ft" $
        "domainType" <~ Core.functionTypeDomain (var "ft") $
        "codomainType" <~ Core.functionTypeCodomain (var "ft") $
        "rest" <~ (peelLambdas @@ var "body" @@ var "codomainType") $
        "restParams" <~ Pairs.first (var "rest") $
        "restInner" <~ Pairs.second (var "rest") $
          pair (Lists.cons (pair (var "paramName") (var "domainType")) (var "restParams")) (var "restInner"),
       _Type_forall>>: lambda "fa" $
        -- Skip through forall to the underlying type
        peelLambdas @@ var "term" @@ Core.forallTypeBody (var "fa")]]

-- =============================================================================
-- Function analysis (using shared CoderUtils)
-- =============================================================================

-- | Analyze a Rust function term, collecting lambdas, type lambdas, lets, and type applications.
-- Uses the same shared infrastructure as the Java and Python coders.
analyzeRustFunction :: TTermDefinition (Term -> Context -> Graph -> Either (InContext Error) (FunctionStructure Graph))
analyzeRustFunction = def "analyzeRustFunction" $
  lambda "term" $ "cx" ~> "g" ~>
    CoderUtils.analyzeFunctionTerm @@ var "cx"
      @@ (lambda "env" $ var "env")          -- getTC: identity (Graph IS the env)
      @@ (lambda "newG" $ lambda "_" $ var "newG")  -- setTC: replace graph
      @@ var "g"
      @@ var "term"

-- =============================================================================
-- Type-directed Rc wrapping
-- =============================================================================

-- | Check if a type is a function type (including through forall wrappers)
isFunctionType :: TTermDefinition (Type -> Bool)
isFunctionType = def "isFunctionType" $
  lambda "t" $
    "dt" <~ (Rewriting.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $ boolean False)
      [_Type_function>>: constant $ boolean True,
       _Type_forall>>: lambda "fa" $
         isFunctionType @@ Core.forallTypeBody (var "fa")]

-- | Encode a lambda term as an Rc-wrapped move closure with pre-cloned captures.
-- Used when a lambda is assigned to a function-typed struct field.
-- Generates: { let cap_x = x.clone(); ... Rc::new(move |params| body) }
-- The input term MUST be a Term_function(Function_lambda(...)).
encodeRcWrappedLambda :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) R.Expression)
encodeRcWrappedLambda = def "encodeRcWrappedLambda" $
  "cx" ~> "g" ~> lambda "term" $
    -- Collect nested lambdas into a multi-param closure
    "collected" <~ (collectLambdaParams @@ var "term") $
    "rawParams" <~ Pairs.first (var "collected") $
    "closureParams" <~ (Lists.map (lambda "pt" $
      "pname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Pairs.first (var "pt"))) $
      "mtyp" <~ Pairs.second (var "pt") $
      record R._ClosureParam [
        R._ClosureParam_pattern>>:
          inject R._Pattern R._Pattern_identifier $
            record R._IdentifierPattern [
              R._IdentifierPattern_name>>: var "pname",
              R._IdentifierPattern_mutable>>: boolean False,
              R._IdentifierPattern_atPattern>>: nothing],
        R._ClosureParam_type>>:
          Maybes.bind (var "mtyp") (lambda "t" $
            Eithers.either_ (constant nothing) (lambda "rt" $ just (var "rt"))
              (encodeType @@ var "cx" @@ var "g" @@ var "t"))])
      (var "rawParams")) $
    "innerBody" <~ Pairs.second (var "collected") $
    "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "innerBody") $
    -- Find free variables in the lambda body (these need pre-cloning for move capture)
    "paramNames" <~ Sets.fromList (Lists.map (lambda "pt" $ wrap _Name (Pairs.first (var "pt"))) (var "rawParams")) $
    "allFreeVars" <~ (Rewriting.freeVariablesInTerm @@ var "innerBody") $
    -- Filter to only unqualified names (local variables, not cross-module refs)
    "capturedVars" <~ Sets.toList (Sets.difference (var "allFreeVars") (var "paramNames")) $
    "localCaptures" <~ Lists.filter (lambda "v" $
      Lists.null (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "v"))))) (var "capturedVars") $
    -- Generate pre-clone let statements: let cap_x = x.clone();
    "preCloneStmts" <~ Lists.map (lambda "v" $
      "vname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ Core.unName (var "v"))) $
      rustLetStmt @@ var "vname" @@ (rustClone @@ (rustExprPath @@ var "vname")))
      (var "localCaptures") $
    -- Build Rc::new(move |params| body)
    "closureExpr" <~ (inject R._Expression R._Expression_closure $
      record R._ClosureExpr [
        R._ClosureExpr_move>>: boolean True,
        R._ClosureExpr_params>>: var "closureParams",
        R._ClosureExpr_returnType>>: nothing,
        R._ClosureExpr_body>>: var "body"]) $
    "rcExpr" <~ (rustCall @@ (rustExprPathSegmented @@ list [string "Rc", string "new"]) @@ list [var "closureExpr"]) $
    -- Wrap everything in a block: { let cap_x = x.clone(); ... Rc::new(move |..| ..) }
    Logic.ifElse (Lists.null (var "preCloneStmts"))
      (right (var "rcExpr"))
      (right (rustBlock @@ var "preCloneStmts" @@ var "rcExpr"))

-- =============================================================================
-- Module entry point
-- | Encode a type for use as a function parameter.
-- For function types, generates `impl Fn(A) -> B` instead of `Rc<dyn Fn(A) -> B>`.
-- This allows callers to pass both bare closures and fn items without Rc wrapping.
encodeParamType :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) R.Type)
encodeParamType = def "encodeParamType" $
  "cx" ~> "g" ~> lambda "t" $
    "dt" <~ (Rewriting.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $
        -- Non-function types: delegate to encodeType
        encodeType @@ var "cx" @@ var "g" @@ var "t")
      [_Type_function>>: lambda "ft" $
        -- Function type: generate impl Fn(A) -> B instead of Rc<dyn Fn(A) -> B>
        "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
        "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
          right (inject R._Type R._Type_implTrait $ list [
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
                          R._ParenthesizedArgs_output>>: just (var "cod")]]]],
            -- Add Clone bound so the parameter can be .clone()'d
            inject R._TypeParamBound R._TypeParamBound_trait $
              record R._TypePath [
                R._TypePath_global>>: boolean False,
                R._TypePath_segments>>: list [
                  record R._PathSegment [
                    R._PathSegment_name>>: string "Clone",
                    R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]]])]

-- =============================================================================

-- | Convert a Hydra module to a map of file paths to Rust source code strings.
moduleToRust :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToRust = def "moduleToRust" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "partitioned" <~ (Schemas.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    "typeItemLists" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "typeItems" <~ Lists.concat (var "typeItemLists") $
    "termItems" <<~ (Eithers.mapList (encodeTermDefinition @@ var "cx" @@ var "g") (var "termDefs")) $
    "allItems" <~ Lists.concat2 (var "typeItems") (var "termItems") $
    "crate" <~ (record R._Crate [R._Crate_items>>: var "allItems"]) $
    "rawCode" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (RustSerdeSource.crateToExpr @@ var "crate"))) $
    -- Post-process: strip self-module prefix so types within the same module use local names
    "ns" <~ (Module.moduleNamespace (var "mod")) $
    "selfPrefix" <~ Strings.cat (list [
      string "crate::",
      Strings.intercalate (string "::")
        (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s")
          (Strings.splitOn (string ".") (Module.unNamespace (var "ns")))),
      string "::"]) $
    -- Replace self-prefix with empty string: splitOn + intercalate = replaceAll
    "code" <~ (Strings.intercalate (string "") (Strings.splitOn (var "selfPrefix") (var "rawCode"))) $
    -- Generate use declarations for module dependencies (excluding self)
    "allDeps" <~ Lists.filter (lambda "dep" $
        Logic.ifElse (Equality.equal (Module.unNamespace (var "dep")) (Module.unNamespace (var "ns")))
          (boolean False) (boolean True))
      (Lists.nub (Lists.concat2
        (Module.moduleTermDependencies (var "mod"))
        (Module.moduleTypeDependencies (var "mod")))) $
    "depUseDecls" <~ Strings.intercalate (string "\n") (Lists.map
      (lambda "dep" $
        Strings.cat (list [
          string "use crate::",
          Strings.intercalate (string "::")
            (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s")
              (Strings.splitOn (string ".") (Module.unNamespace (var "dep")))),
          string "::*;"]))
      (var "allDeps")) $
    "stdImports" <~ (string "#![allow(unused_imports)]\n#![allow(non_camel_case_types)]\n\nuse std::collections::BTreeMap;\nuse std::collections::BTreeSet;\nuse std::rc::Rc;\nuse ordered_float::OrderedFloat;\nuse crate::Either;\n") $
    "header" <~ Logic.ifElse (Lists.null (var "allDeps"))
      (Strings.cat2 (var "stdImports") (string "\n"))
      (Strings.cat (list [var "stdImports", var "depUseDecls", string "\n\n"])) $
    "finalCode" <~ Strings.cat2 (var "header") (var "code") $
    "filePath" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "rs") @@ var "ns") $
      right (Maps.singleton (var "filePath") (var "finalCode"))
