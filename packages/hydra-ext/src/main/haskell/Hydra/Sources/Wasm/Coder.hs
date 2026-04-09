-- | WebAssembly code generator in Hydra DSL.
-- This module provides DSL versions of WAT code generation functions.
-- Type definitions are mapped to memory layout conventions; term definitions are mapped to WASM functions.

module Hydra.Sources.Wasm.Coder where

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
import qualified Hydra.Sources.Kernel.Terms.Analysis       as Analysis
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for WASM AST
import qualified Hydra.Wasm.Syntax as W
import qualified Hydra.Sources.Wasm.Syntax as WasmSyntax
import qualified Hydra.Sources.Wasm.Serde as WasmSerdeSource
import qualified Hydra.Sources.Wasm.Language as WasmLanguageSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.wasm.coder"

module_ :: Module
module_ = Module ns definitions
    [moduleNamespace WasmSerdeSource.module_, moduleNamespace WasmLanguageSource.module_,
      Analysis.ns, Formatting.ns, Names.ns, Strip.ns, Variables.ns, Environment.ns, Lexical.ns, SerializationSource.ns]
    (WasmSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "WebAssembly code generator: converts Hydra type and term modules to WAT source code"
  where
    definitions = [
      toDefinition encodeApplication,
      toDefinition encodeValType,
      toDefinition encodeLiteralType,
      toDefinition encodeLiteral,
      toDefinition encodeType,
      toDefinition encodeTerm,
      toDefinition encodeFunction,
      toDefinition encodeElimination,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTermDefinition,
      toDefinition extractLambdaParams,
      toDefinition extractParamTypes,
      toDefinition collectInstructionLocals,
      toDefinition collectCallTargets,
      toDefinition moduleToWasm]


-- =============================================================================
-- Value type encoding
-- =============================================================================

-- | Map a Hydra type to a WASM value type
encodeValType :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) W.ValType)
encodeValType = def "encodeValType" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "typ") (Just $
      -- Default: compound types are represented as i32 (memory pointer)
      right (inject W._ValType W._ValType_i32 unit))
    [_Type_annotated>>: lambda "at" $
       encodeValType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
     _Type_application>>: lambda "at" $
       encodeValType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at"),
     _Type_literal>>: lambda "lt" $
       right (encodeLiteralType @@ var "lt"),
     _Type_unit>>: constant $
       right (inject W._ValType W._ValType_i32 unit),
     _Type_void>>: constant $
       right (inject W._ValType W._ValType_i32 unit),
     _Type_forall>>: lambda "fa" $
       encodeValType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]


-- =============================================================================
-- Literal type encoding
-- =============================================================================

-- | Encode a Hydra literal type as a WASM value type
encodeLiteralType :: TTermDefinition (LiteralType -> W.ValType)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      inject W._ValType W._ValType_i32 unit, -- pointer to memory
    _LiteralType_boolean>>: constant $
      inject W._ValType W._ValType_i32 unit, -- 0 or 1
    _LiteralType_float>>: lambda "ft" $
      cases _FloatType (var "ft") Nothing [
        _FloatType_bigfloat>>: constant $ inject W._ValType W._ValType_f64 unit,
        _FloatType_float32>>: constant $ inject W._ValType W._ValType_f32 unit,
        _FloatType_float64>>: constant $ inject W._ValType W._ValType_f64 unit],
    _LiteralType_integer>>: lambda "it" $
      cases _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $ inject W._ValType W._ValType_i64 unit,
        _IntegerType_int8>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_int16>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_int32>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_int64>>: constant $ inject W._ValType W._ValType_i64 unit,
        _IntegerType_uint8>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_uint16>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_uint32>>: constant $ inject W._ValType W._ValType_i32 unit,
        _IntegerType_uint64>>: constant $ inject W._ValType W._ValType_i64 unit],
    _LiteralType_string>>: constant $
      inject W._ValType W._ValType_i32 unit] -- pointer to memory


-- =============================================================================
-- Literal value encoding
-- =============================================================================

-- | Encode a Hydra literal value as a WASM const instruction
encodeLiteral :: TTermDefinition (Literal -> W.Instruction)
encodeLiteral = def "encodeLiteral" $
  lambda "lit" $ cases _Literal (var "lit") Nothing [
    _Literal_boolean>>: lambda "b" $
      inject W._Instruction W._Instruction_const $
        inject W._ConstValue W._ConstValue_i32 $
          Logic.ifElse (var "b") (int32 1) (int32 0),
    _Literal_string>>: lambda "s" $
      -- Strings in WASM are stored in linear memory.
      -- Emit a raw instruction showing the string value as a comment, with i32.const 0 placeholder.
      -- A full implementation would allocate memory and return a (pointer, length) pair.
      inject W._Instruction W._Instruction_raw $
        Strings.cat (list [string "i32.const 0 ;; string: \"", var "s", string "\""]),
    _Literal_float>>: lambda "fv" $
      cases _FloatValue (var "fv") Nothing [
        _FloatValue_float32>>: lambda "f" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_f32 (var "f"),
        _FloatValue_float64>>: lambda "f" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_f64 (var "f"),
        _FloatValue_bigfloat>>: lambda "f" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_f64 (Literals.bigfloatToFloat64 (var "f"))],
    _Literal_integer>>: lambda "iv" $
      cases _IntegerValue (var "iv") Nothing [
        _IntegerValue_int8>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.int8ToBigint (var "i"))),
        _IntegerValue_int16>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.int16ToBigint (var "i"))),
        _IntegerValue_int32>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (var "i"),
        _IntegerValue_int64>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i64 (var "i"),
        _IntegerValue_uint8>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint8ToBigint (var "i"))),
        _IntegerValue_uint16>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint16ToBigint (var "i"))),
        _IntegerValue_uint32>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint32ToBigint (var "i"))),
        _IntegerValue_uint64>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i64 (Literals.bigintToInt64 (Literals.uint64ToBigint (var "i"))),
        _IntegerValue_bigint>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i64 (Literals.bigintToInt64 (var "i"))]]


-- =============================================================================
-- Type encoding (for function signatures)
-- =============================================================================

-- | Encode a Hydra type as a list of WASM result types (for function signatures).
-- For function types, returns the innermost codomain (after stripping all arrows).
encodeType :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) [W.ValType])
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "typ") (Just $
      -- Default: single i32 result (pointer)
      right (list [inject W._ValType W._ValType_i32 unit]))
    [_Type_annotated>>: lambda "at" $
       encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
     _Type_function>>: lambda "ft" $
       -- For function types, skip domain and recurse into codomain
       encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft"),
     _Type_unit>>: constant $
       right (list ([] :: [TTerm W.ValType])),
     _Type_void>>: constant $
       right (list ([] :: [TTerm W.ValType])),
     _Type_literal>>: lambda "lt" $
       right (list [encodeLiteralType @@ var "lt"]),
     _Type_forall>>: lambda "fa" $
       encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]


-- =============================================================================
-- Application encoding
-- =============================================================================

-- | Encode a function application by gathering all arguments from nested applications,
-- then dispatching based on the head function type.
-- This produces correct WASM stack ordering: args pushed first, then call.
encodeApplication :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) [W.Instruction])
encodeApplication = def "encodeApplication" $
  "cx" ~> "g" ~> lambda "term" $
    "gathered" <~ (Analysis.gatherArgs @@ var "term" @@ list ([] :: [TTerm Term])) $
    "fun" <~ Pairs.first (var "gathered") $
    "args" <~ Pairs.second (var "gathered") $
    "dfun" <~ (Strip.deannotateTerm @@ var "fun") $
    -- Encode all arguments
    "encodedArgs" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "args")) $
    "argInstrs" <~ Lists.concat (var "encodedArgs") $
    -- Dispatch based on the head function
    cases _Term (var "dfun") (Just $
      -- Default: encode the head function and append after args
      "funInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fun") $
        right (Lists.concat2 (var "argInstrs") (var "funInstrs")))
    [_Term_variable>>: lambda "name" $
       -- Variable head: push all args, then call
       "rawName" <~ Core.unName (var "name") $
       "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ var "rawName") $
       Logic.ifElse (Lists.null (Lists.tail (Strings.splitOn (string ".") (var "rawName"))))
         -- Local variable applied to args: push args, then local.get, then call_indirect (unsupported)
         -- For now, push args then local.get (best effort)
         (right (Lists.concat2 (var "argInstrs")
           (list [inject W._Instruction W._Instruction_localGet (var "lname")])))
         -- Cross-module call: push all args, then call
         (right (Lists.concat2 (var "argInstrs")
           (list [inject W._Instruction W._Instruction_call (var "lname")]))),
     _Term_function>>: lambda "fn" $
       cases _Function (var "fn") (Just $
         "funInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fun") $
           right (Lists.concat2 (var "argInstrs") (var "funInstrs")))
       [_Function_elimination>>: lambda "elim" $
         -- Elimination: first arg is the scrutinee, remaining args applied after
         Logic.ifElse (Lists.null (var "args"))
           (encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@ list ([] :: [TTerm W.Instruction]))
           (encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@ var "argInstrs"),
        _Function_lambda>>: lambda "lam" $
         -- Lambda applied to args: encode the body (params will be bound via let or inline)
         encodeTerm @@ var "cx" @@ var "g" @@ Core.lambdaBody (var "lam")]]


-- =============================================================================
-- Term encoding
-- =============================================================================

-- | Encode a Hydra term as a list of WASM instructions
encodeTerm :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) [W.Instruction])
encodeTerm = def "encodeTerm" $
  "cx" ~> "g" ~> lambda "term" $
    cases _Term (var "term") (Just $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "unexpected term variant in WASM encoding") (var "cx"))
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.annotatedTermBody (var "at"),
     _Term_application>>: lambda "app" $
       encodeApplication @@ var "cx" @@ var "g" @@ Core.termApplication (var "app"),
     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $
           "sl" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "l") $
             -- Left: tag 0, then value
             right (Lists.concat2
               (list [inject W._Instruction W._Instruction_const $
                 inject W._ConstValue W._ConstValue_i32 (int32 0)])
               (var "sl")))
         (lambda "r" $
           "sr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "r") $
             -- Right: tag 1, then value
             right (Lists.concat2
               (list [inject W._Instruction W._Instruction_const $
                 inject W._ConstValue W._ConstValue_i32 (int32 1)])
               (var "sr")))
         (var "e"),
     _Term_function>>: lambda "fun" $
       encodeFunction @@ var "cx" @@ var "g" @@ var "fun",
     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       "bindInstrs" <<~ (Eithers.mapList
         (lambda "b" $
           "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.bindingName (var "b"))) $
           "bval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
             right (Lists.concat2 (var "bval")
               (list [inject W._Instruction W._Instruction_localSet (var "bname")])))
         (var "bindings")) $
       "bodyInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "body") $
         right (Lists.concat2 (Lists.concat (var "bindInstrs")) (var "bodyInstrs")),
     _Term_list>>: lambda "els" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "els")) $
         -- Push element count, then each element's instructions
         right (Lists.concat2
           (list [inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (Lists.length (var "els")),
           inject W._Instruction W._Instruction_raw (string ";; list elements follow")])
           (Lists.concat (var "sels"))),
     _Term_literal>>: lambda "lit" $
       right (list [encodeLiteral @@ var "lit"]),
     _Term_map>>: lambda "m" $
       "pairs" <<~ (Eithers.mapList
         (lambda "entry" $
           "k" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "entry")) $
           "v" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "entry")) $
             right (Lists.concat2 (var "k") (var "v")))
         (Maps.toList (var "m"))) $
         right (Lists.concat2
           (list [inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (Lists.length (Maps.toList (var "m"))),
           inject W._Instruction W._Instruction_raw (string ";; map entries follow")])
           (Lists.concat (var "pairs"))),
     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         -- Nothing: push 0 (null pointer / tag for None)
         (right (list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (lambda "val" $
           encodeTerm @@ var "cx" @@ var "g" @@ var "val"),
     _Term_pair>>: lambda "p" $
       "fInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "p")) $
       "sInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "p")) $
         right (Lists.concat2 (var "fInstrs") (var "sInstrs")),
     _Term_record>>: lambda "rec" $
       "fields" <~ Core.recordFields (var "rec") $
       "fieldInstrs" <<~ (Eithers.mapList
         (lambda "f" $
           encodeTerm @@ var "cx" @@ var "g" @@ Core.fieldTerm (var "f"))
         (var "fields")) $
         right (Lists.concat (var "fieldInstrs")),
     _Term_set>>: lambda "s" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (Sets.toList (var "s"))) $
         right (Lists.concat2
           (list [inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (Lists.length (Sets.toList (var "s"))),
           inject W._Instruction W._Instruction_raw (string ";; set elements follow")])
           (Lists.concat (var "sels"))),
     _Term_union>>: lambda "inj" $
       "field" <~ Core.injectionField (var "inj") $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Strip.deannotateTerm @@ var "fterm") $
       "isUnit" <~ (cases _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.null (Core.recordFields (var "rt"))]) $
       Logic.ifElse (var "isUnit")
         (right (list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (encodeTerm @@ var "cx" @@ var "g" @@ var "fterm"),
     _Term_unit>>: constant $
       right (list ([] :: [TTerm W.Instruction])),
     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.typeApplicationTermBody (var "ta"),
     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.typeLambdaBody (var "tl"),
     _Term_variable>>: lambda "name" $
       "rawName" <~ Core.unName (var "name") $
       "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ var "rawName") $
       -- Qualified names (containing ".") are cross-module references → emit call
       -- Unqualified names are local variables → emit local.get
       Logic.ifElse (Lists.null (Lists.tail (Strings.splitOn (string ".") (var "rawName"))))
         (right (list [inject W._Instruction W._Instruction_localGet (var "lname")]))
         (right (list [inject W._Instruction W._Instruction_call (var "lname")])),
     _Term_wrap>>: lambda "wt" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.wrappedTermBody (var "wt")]


-- =============================================================================
-- Function encoding
-- =============================================================================

-- | Encode a Hydra function as WASM instructions
encodeFunction :: TTermDefinition (Context -> Graph -> Function -> Either (InContext Error) [W.Instruction])
encodeFunction = def "encodeFunction" $
  "cx" ~> "g" ~> lambda "fun" $
    cases _Function (var "fun") Nothing [
      _Function_lambda>>: lambda "lam" $
        -- In WASM, lambdas are emitted as function bodies; the parameter is accessed via local.get
        encodeTerm @@ var "cx" @@ var "g" @@ Core.lambdaBody (var "lam"),
      _Function_elimination>>: lambda "elim" $
        encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@ list ([] :: [TTerm W.Instruction])]


-- =============================================================================
-- Elimination encoding
-- =============================================================================

-- | Encode a Hydra elimination as WASM instructions.
-- Takes scrutinee instructions to place before the dispatch.
encodeElimination :: TTermDefinition (Context -> Graph -> Elimination -> [W.Instruction] -> Either (InContext Error) [W.Instruction])
encodeElimination = def "encodeElimination" $
  "cx" ~> "g" ~> lambda "elim" $ lambda "scrutineeInstrs" $
    cases _Elimination (var "elim") Nothing [
      _Elimination_record>>: lambda "proj" $
        "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.projectionField (var "proj"))) $
        -- Record projection: push scrutinee (the record pointer) then emit a comment.
        -- In a full implementation, this would calculate the memory offset for the field.
        -- For now, the record pointer passes through as the projected value (i32 pointer).
        right (Lists.concat2 (var "scrutineeInstrs")
          (list [
            inject W._Instruction W._Instruction_raw (Strings.cat (list [
              string ";; project field: ", var "fname"]))])),
      _Elimination_union>>: lambda "cs" $
        "tname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Names.localNameOf @@ Core.caseStatementTypeName (var "cs"))) $
        "caseFields" <~ Core.caseStatementCases (var "cs") $
        -- Encode each case branch body
        "arms" <<~ (Eithers.mapList
          (lambda "cf" $
            "cfname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldName (var "cf"))) $
            "cfterm" <~ Core.fieldTerm (var "cf") $
            "armBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@
              (Core.termApplication (Core.application (var "cfterm")
                (Core.termVariable (wrap _Name (string "v")))))) $
              right (pair (var "cfname") (var "armBody")))
          (var "caseFields")) $
        -- Block-based union dispatch using the standard WASM br_table pattern:
        --   (block $end (result i32)
        --     (block $armN)         ;; empty dispatch targets
        --       ...
        --       (block $arm0)
        --         scrutinee
        --         br_table $arm0 $arm1 ... $armN
        --       )
        --       arm0 body
        --       br $end
        --     )
        --     ...
        --     armN body     ;; last arm falls through
        --   )
        -- When br_table dispatches to $armK, it exits blocks up to $armK,
        -- then armK's body executes after that block.
        "armLabels" <~ Lists.map (unaryFunction Pairs.first) (var "arms") $
        "endLabel" <~ (Strings.cat2 (string "end_") (var "tname")) $
        -- Default for br_table: use last arm label (for out-of-range tags)
        "defaultLabel" <~ Logic.ifElse (Lists.null (var "armLabels"))
          (var "endLabel")
          (Lists.last (var "armLabels")) $
        -- Build nested empty dispatch blocks from inside out.
        -- The innermost block contains the scrutinee + br_table.
        -- Each subsequent block wraps the inner blocks, then has its arm body + br $end after.
        "innerDispatch" <~ Lists.concat2
          (var "scrutineeInstrs")
          (list [inject W._Instruction W._Instruction_brTable $
            record W._BrTableArgs [
              W._BrTableArgs_labels>>: var "armLabels",
              W._BrTableArgs_default>>: var "defaultLabel"]]) $
        -- Build from inside out using foldl over arms.
        -- For each arm: wrap accumulated code in an empty block, then append arm body + br $end.
        "dispatch" <~ Lists.foldl
          (lambda "acc" $ lambda "arm" $
            "label" <~ Pairs.first (var "arm") $
            "body" <~ Pairs.second (var "arm") $
              Lists.concat (list [
                list [inject W._Instruction W._Instruction_block $
                  record W._BlockInstruction [
                    W._BlockInstruction_label>>: just (var "label"),
                    W._BlockInstruction_blockType>>: inject W._BlockType W._BlockType_empty unit,
                    W._BlockInstruction_body>>: var "acc"]],
                var "body",
                list [inject W._Instruction W._Instruction_br (var "endLabel")]]))
          (var "innerDispatch")
          (var "arms") $
        -- Wrap everything in the outermost end block with result type
          right (list [inject W._Instruction W._Instruction_block $
            record W._BlockInstruction [
              W._BlockInstruction_label>>: just (var "endLabel"),
              W._BlockInstruction_blockType>>: inject W._BlockType W._BlockType_value (inject W._ValType W._ValType_i32 unit),
              W._BlockInstruction_body>>: var "dispatch"]]),
      _Elimination_wrap>>: lambda "_" $
        -- Unwrap: identity in WASM (the wrapper is erased)
        right (list [inject W._Instruction W._Instruction_nop unit])]


-- =============================================================================
-- Type definition encoding
-- =============================================================================

-- | Encode a Hydra type definition as WASM module fields.
-- Types are erased at runtime in WASM, but we emit function type definitions
-- for types that represent function signatures (useful for call_indirect).
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> TypeDefinition -> Either (InContext Error) [W.ModuleField])
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.typeDefinitionName (var "tdef") $
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Names.localNameOf @@ var "name")) $
    "typ" <~ (Core.typeSchemeType $ Packaging.typeDefinitionType (var "tdef")) $
    "dtyp" <~ (Strip.deannotateType @@ var "typ") $
    -- Emit a type section entry for function types
    cases _Type (var "dtyp") (Just $
      -- Non-function types are erased
      right (list ([] :: [TTerm W.ModuleField])))
    [_Type_function>>: lambda "ft" $
      "paramTypes" <<~ (extractParamTypes @@ var "cx" @@ var "g" @@ var "typ") $
      "resultTypes" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
        right (list [
          inject W._ModuleField W._ModuleField_type $
            record W._TypeDef [
              W._TypeDef_name>>: just (var "lname"),
              W._TypeDef_type>>: record W._FuncType [
                W._FuncType_params>>: var "paramTypes",
                W._FuncType_results>>: var "resultTypes"]]])]


-- =============================================================================
-- Term definition encoding
-- =============================================================================

-- | Extract parameter names from nested lambdas, returning (params, innerBody)
extractLambdaParams :: TTermDefinition (Term -> ([Name], Term))
extractLambdaParams = def "extractLambdaParams" $
  lambda "term" $
    "stripped" <~ (Strip.deannotateTerm @@ var "term") $
    cases _Term (var "stripped") (Just $ pair (list ([] :: [TTerm Name])) (var "term")) [
      _Term_function>>: lambda "fun" $
        cases _Function (var "fun") (Just $ pair (list ([] :: [TTerm Name])) (var "term")) [
          _Function_lambda>>: lambda "lam" $
            "paramName" <~ Core.lambdaParameter (var "lam") $
            "body" <~ Core.lambdaBody (var "lam") $
            "inner" <~ (extractLambdaParams @@ var "body") $
              pair
                (Lists.cons (var "paramName") (Pairs.first (var "inner")))
                (Pairs.second (var "inner")),
          -- Bare elimination: generate a synthetic parameter name
          -- The elimination itself becomes the body (applied to the synthetic param)
          _Function_elimination>>: lambda "_elim" $
            pair (list [wrap _Name (string "arg_0")]) (var "term")],
      _Term_typeLambda>>: lambda "tl" $
        extractLambdaParams @@ Core.typeLambdaBody (var "tl"),
      _Term_typeApplication>>: lambda "ta" $
        extractLambdaParams @@ Core.typeApplicationTermBody (var "ta")]

-- | Collect all local variable names referenced by local.get and local.set in a list of instructions.
-- Recurses into block, loop, and if bodies.
collectInstructionLocals :: TTermDefinition ([W.Instruction] -> S.Set String)
collectInstructionLocals = def "collectInstructionLocals" $
  lambda "instrs" $
    Lists.foldl
      (lambda "acc" $ lambda "instr" $
        cases W._Instruction (var "instr") (Just $ var "acc") [
          W._Instruction_localGet>>: lambda "v" $ Sets.insert (var "v") (var "acc"),
          W._Instruction_localSet>>: lambda "v" $ Sets.insert (var "v") (var "acc"),
          W._Instruction_localTee>>: lambda "v" $ Sets.insert (var "v") (var "acc"),
          W._Instruction_block>>: lambda "b" $
            Sets.union (var "acc") (collectInstructionLocals @@ (project W._BlockInstruction W._BlockInstruction_body @@ var "b")),
          W._Instruction_loop>>: lambda "b" $
            Sets.union (var "acc") (collectInstructionLocals @@ (project W._BlockInstruction W._BlockInstruction_body @@ var "b")),
          W._Instruction_if>>: lambda "i" $
            Sets.union (Sets.union (var "acc")
              (collectInstructionLocals @@ (project W._IfInstruction W._IfInstruction_then @@ var "i")))
              (collectInstructionLocals @@ (project W._IfInstruction W._IfInstruction_else @@ var "i"))])
      (Sets.empty :: TTerm (S.Set String))
      (var "instrs")

-- | Collect all call targets from a list of instructions.
-- Recurses into block, loop, and if bodies.
collectCallTargets :: TTermDefinition ([W.Instruction] -> S.Set String)
collectCallTargets = def "collectCallTargets" $
  lambda "instrs" $
    Lists.foldl
      (lambda "acc" $ lambda "instr" $
        cases W._Instruction (var "instr") (Just $ var "acc") [
          W._Instruction_call>>: lambda "v" $ Sets.insert (var "v") (var "acc"),
          W._Instruction_block>>: lambda "b" $
            Sets.union (var "acc") (collectCallTargets @@ (project W._BlockInstruction W._BlockInstruction_body @@ var "b")),
          W._Instruction_loop>>: lambda "b" $
            Sets.union (var "acc") (collectCallTargets @@ (project W._BlockInstruction W._BlockInstruction_body @@ var "b")),
          W._Instruction_if>>: lambda "i" $
            Sets.union (Sets.union (var "acc")
              (collectCallTargets @@ (project W._IfInstruction W._IfInstruction_then @@ var "i")))
              (collectCallTargets @@ (project W._IfInstruction W._IfInstruction_else @@ var "i"))])
      (Sets.empty :: TTerm (S.Set String))
      (var "instrs")

-- | Extract parameter types from a function type, returning a list of domain types
extractParamTypes :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) [W.ValType])
extractParamTypes = def "extractParamTypes" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "typ") (Just $ right (list ([] :: [TTerm W.ValType]))) [
      _Type_function>>: lambda "ft" $
        "domType" <<~ (encodeValType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
        "rest" <<~ (extractParamTypes @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
          right (Lists.cons (var "domType") (var "rest")),
      _Type_forall>>: lambda "fa" $
        extractParamTypes @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]

-- | Encode a Hydra term definition as a WASM function
encodeTermDefinition :: TTermDefinition (Context -> Graph -> TermDefinition -> Either (InContext Error) W.ModuleField)
encodeTermDefinition = def "encodeTermDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.termDefinitionName (var "tdef") $
    "term" <~ Packaging.termDefinitionTerm (var "tdef") $
    -- Use fully qualified name for the function (matches call sites)
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "name")) $
    "typ" <~ Maybes.maybe
      (Core.typeUnit)
      (unaryFunction Core.typeSchemeType)
      (Packaging.termDefinitionType (var "tdef")) $
    -- Extract lambda parameters and inner body
    "extracted" <~ (extractLambdaParams @@ var "term") $
    "paramNames" <~ Pairs.first (var "extracted") $
    "innerBody" <~ Pairs.second (var "extracted") $
    -- Get parameter types from the function type
    "paramTypes" <<~ (extractParamTypes @@ var "cx" @@ var "g" @@ var "typ") $
    -- If there are more param types than param names, use i32 defaults for extras
    "effectiveParamTypes" <~ Logic.ifElse
      (Logic.and (Logic.not (Lists.null (var "paramNames"))) (Lists.null (var "paramTypes")))
      (Lists.map (constant $ inject W._ValType W._ValType_i32 unit) (var "paramNames"))
      (var "paramTypes") $
    -- Build WASM params by zipping names with types
    "wasmParams" <~ Lists.map
      (lambda "pair" $
        record W._Param [
          W._Param_name>>: just (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Pairs.first (var "pair"))),
          W._Param_type>>: Pairs.second (var "pair")])
      (Lists.zip (var "paramNames") (var "effectiveParamTypes")) $
    -- Get the result type (strip function type layers to get the codomain)
    "resultTypes" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
    -- Encode body: for bare eliminations (detected by extractLambdaParams producing synthetic param "arg_0"),
    -- pass local.get $arg_0 as the scrutinee to encodeElimination.
    "dBody" <~ (Strip.deannotateTerm @@ var "innerBody") $
    "bodyInstrs" <<~ (cases _Term (var "dBody") (Just $
        encodeTerm @@ var "cx" @@ var "g" @@ var "innerBody")
      [_Term_function>>: lambda "fn" $
        cases _Function (var "fn") (Just $
          encodeTerm @@ var "cx" @@ var "g" @@ var "innerBody")
        [_Function_elimination>>: lambda "elim" $
          -- If extractLambdaParams found a synthetic param, use it as scrutinee
          Logic.ifElse (Lists.null (var "paramNames"))
            (encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@ list ([] :: [TTerm W.Instruction]))
            (encodeElimination @@ var "cx" @@ var "g" @@ var "elim" @@
              list [inject W._Instruction W._Instruction_localGet
                (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Lists.head (var "paramNames")))])]]) $
    -- Collect all local variable names referenced in the body instructions
    "referencedLocals" <~ (collectInstructionLocals @@ var "bodyInstrs") $
    -- Subtract parameter names to get the undeclared locals
    "paramNameStrs" <~ Lists.map
      (lambda "pn" $ Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "pn"))
      (var "paramNames") $
    "undeclaredLocals" <~ Sets.toList (Sets.difference (var "referencedLocals") (Sets.fromList (var "paramNameStrs"))) $
    "wasmLocals" <~ Lists.map
      (lambda "ln" $
        record W._FuncLocal [
          W._FuncLocal_name>>: just (var "ln"),
          -- Default to i32 for locals (all compound types are pointers)
          W._FuncLocal_type>>: inject W._ValType W._ValType_i32 unit])
      (var "undeclaredLocals") $
      right (inject W._ModuleField W._ModuleField_func $
        record W._Func [
          W._Func_name>>: just (var "lname"),
          W._Func_typeUse>>: record W._TypeUse [
            W._TypeUse_index>>: nothing,
            W._TypeUse_params>>: var "wasmParams",
            W._TypeUse_results>>: var "resultTypes"],
          W._Func_locals>>: var "wasmLocals",
          W._Func_body>>: var "bodyInstrs"])


-- =============================================================================
-- Module entry point
-- =============================================================================

-- | Convert a Hydra module to a map of file paths to WAT source code strings.
moduleToWasm :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToWasm = def "moduleToWasm" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    "typeFields" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "termFields" <<~ (Eithers.mapList (encodeTermDefinition @@ var "cx" @@ var "g") (var "termDefs")) $
    "allFields" <~ Lists.concat2 (Lists.concat (var "typeFields")) (var "termFields") $
    -- Add a memory definition (1 page = 64KB) for compound types
    "memField" <~ (inject W._ModuleField W._ModuleField_memory $
      record W._MemoryDef [
        W._MemoryDef_name>>: just (string "memory"),
        W._MemoryDef_limits>>: record W._Limits [
          W._Limits_min>>: int32 1,
          W._Limits_max>>: nothing]]) $
    -- Add memory export
    "memExport" <~ (inject W._ModuleField W._ModuleField_export $
      record W._ExportDef [
        W._ExportDef_name>>: string "memory",
        W._ExportDef_desc>>: inject W._ExportDesc W._ExportDesc_memory (string "memory")]) $
    -- Build function exports for each term definition
    "funcExports" <~ Lists.map
      (lambda "td" $
        "ename" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Packaging.termDefinitionName (var "td"))) $
        inject W._ModuleField W._ModuleField_export $
          record W._ExportDef [
            W._ExportDef_name>>: var "ename",
            W._ExportDef_desc>>: inject W._ExportDesc W._ExportDesc_func (var "ename")])
      (var "termDefs") $
    -- Collect all call targets from all function bodies to generate imports
    "allBodyInstrs" <~ Lists.concat (Lists.map
      (lambda "tf" $
        cases W._ModuleField (var "tf") (Just $ list ([] :: [TTerm W.Instruction])) [
          W._ModuleField_func>>: lambda "fn" $ project W._Func W._Func_body @@ var "fn"])
      (var "termFields")) $
    "allCallTargets" <~ (collectCallTargets @@ var "allBodyInstrs") $
    -- Local function names (defined in this module)
    "localFuncNames" <~ Sets.fromList (Lists.map
      (lambda "td" $
        Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Packaging.termDefinitionName (var "td")))
      (var "termDefs")) $
    -- External call targets = all calls minus local functions
    "externalCalls" <~ Sets.toList (Sets.difference (var "allCallTargets") (var "localFuncNames")) $
    -- Generate import declarations for external functions
    -- Use module name derived from the function's namespace prefix
    "importFields" <~ Lists.map
      (lambda "fname" $
        "parts" <~ (Strings.splitOn (string ".") (var "fname")) $
        "modName" <~ Strings.intercalate (string ".") (Lists.reverse (Lists.tail (Lists.reverse (var "parts")))) $
          inject W._ModuleField W._ModuleField_import $
            record W._ImportDef [
              W._ImportDef_module>>: var "modName",
              W._ImportDef_name>>: var "fname",
              W._ImportDef_desc>>: inject W._ImportDesc W._ImportDesc_func $
                record W._ImportFunc [
                  W._ImportFunc_name>>: just (var "fname"),
                  W._ImportFunc_typeUse>>: record W._TypeUse [
                    W._TypeUse_index>>: nothing,
                    W._TypeUse_params>>: list [record W._Param [
                      W._Param_name>>: nothing,
                      W._Param_type>>: inject W._ValType W._ValType_i32 unit]],
                    W._TypeUse_results>>: list [inject W._ValType W._ValType_i32 unit]]]])
      (var "externalCalls") $
    "wasmMod" <~ (record W._Module [
      W._Module_name>>: nothing,
      W._Module_fields>>: Lists.concat (list [
        var "importFields",
        list [var "memField", var "memExport"],
        var "funcExports",
        var "allFields"])]) $
    "code" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (WasmSerdeSource.moduleToExpr @@ var "wasmMod"))) $
    "filePath" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "wat") @@ (Packaging.moduleNamespace (var "mod"))) $
      right (Maps.singleton (var "filePath") (var "code"))
