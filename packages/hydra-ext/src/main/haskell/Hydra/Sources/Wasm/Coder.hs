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
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Graph                            as DslGraph
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
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
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
      Analysis.ns, Formatting.ns, Names.ns, Rewriting.ns, Strip.ns, Variables.ns, Environment.ns, Lexical.ns, SerializationSource.ns]
    (WasmSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "WebAssembly code generator: converts Hydra type and term modules to WAT source code"
  where
    definitions = [
      toDefinition buildFieldOffsets,
      toDefinition buildFunctionSignatures,
      toDefinition buildStringOffsets,
      toDefinition buildVariantIndexes,
      toDefinition clampValTypesToI32,
      toDefinition collectCallTargets,
      toDefinition collectInstructionLocals,
      toDefinition collectStrings,
      toDefinition encodeApplication,
      toDefinition encodeCases,
      toDefinition encodeLiteral,
      toDefinition encodeLiteralType,
      toDefinition encodeProjection,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeValType,
      toDefinition extractLambdaParams,
      toDefinition extractParamTypes,
      toDefinition extractSignature,
      toDefinition hexEscapeString,
      toDefinition moduleToWasm,
      toDefinition stringDataSegment]


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
    _Literal_string>>: lambda "_s" $
      -- Strings in WASM are stored in linear memory.
      -- This is a placeholder: emit i32.const 0 (null pointer).
      -- A full implementation would allocate memory and return a (pointer, length) pair.
      -- NOTE: we deliberately do not embed the string value as a comment, since the
      -- underlying string may contain raw newlines which would break WAT parsing.
      inject W._Instruction W._Instruction_const $
        inject W._ConstValue W._ConstValue_i32 (int32 0),
    -- Under the uniform 1-i32-value-per-term placeholder convention, non-i32 literals
    -- (f32, f64, i64) would push a value of the wrong type for enclosing blocks declared
    -- `(result i32)`. Emit `i32.const 0` for all non-i32 literals to maintain uniformity.
    -- Runtime semantic correctness is deferred.
    _Literal_float>>: lambda "_fv" $
      inject W._Instruction W._Instruction_const $
        inject W._ConstValue W._ConstValue_i32 (int32 0),
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
        _IntegerValue_int64>>: lambda "_i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (int32 0),
        _IntegerValue_uint8>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint8ToBigint (var "i"))),
        _IntegerValue_uint16>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint16ToBigint (var "i"))),
        _IntegerValue_uint32>>: lambda "i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (Literals.bigintToInt32 (Literals.uint32ToBigint (var "i"))),
        _IntegerValue_uint64>>: lambda "_i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (int32 0),
        _IntegerValue_bigint>>: lambda "_i" $
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (int32 0)]]


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

-- | Encode a function application. Each external call is emitted as "evaluate args for
-- side effect, drop them, push a single i32 placeholder arg, call". This maintains the
-- invariant that every encoded term produces exactly one stack value, while matching the
-- `(param i32) (result i32)` signature of our import declarations. Correct runtime
-- semantics (actual arg passing) is out of scope for this pass — we're producing
-- syntactically valid WAT only.
encodeApplication :: TTermDefinition (Context -> Graph -> M.Map String Int -> M.Map Name [(Name, Int)] -> M.Map Name [(Name, Int)] -> M.Map String ([W.ValType], [W.ValType]) -> Term -> Either (InContext Error) [W.Instruction])
encodeApplication = def "encodeApplication" $
  "cx" ~> "g" ~> "stringOffsets" ~> "fieldOffsets" ~> "variantIndexes" ~> "funcSigs" ~> lambda "term" $
    "gathered" <~ (Analysis.gatherArgs @@ var "term" @@ list ([] :: [TTerm Term])) $
    "fun" <~ Pairs.first (var "gathered") $
    "args" <~ Pairs.second (var "gathered") $
    "dfun" <~ (Strip.deannotateTerm @@ var "fun") $
    -- Encode each argument, leaving its value on the stack (no drop). This produces
    -- the real argument values in left-to-right order for multi-param calls.
    "realArgInstrs" <<~ (Eithers.mapList
      (lambda "a" $
        encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "a")
      (var "args")) $
    "flatRealArgInstrs" <~ Lists.concat (var "realArgInstrs") $
    -- Encode-and-drop version for cases where we don't want the values (local vars, etc.)
    "droppedArgInstrs" <~ Lists.concat (Lists.map
      (lambda "ai" $ Lists.concat2 (var "ai")
        (list [inject W._Instruction W._Instruction_drop unit]))
      (var "realArgInstrs")) $
    -- Dispatch based on the head function
    cases _Term (var "dfun") (Just $
      -- Default: encode the head function, ignore it (drop), then return placeholder.
      "funInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "fun") $
        right (Lists.concat (list [
          var "droppedArgInstrs",
          var "funInstrs",
          list [inject W._Instruction W._Instruction_drop unit,
                inject W._Instruction W._Instruction_const $
                  inject W._ConstValue W._ConstValue_i32 (int32 0)]])))
    [_Term_variable>>: lambda "name" $
       "rawName" <~ Core.unName (var "name") $
       "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ var "rawName") $
       Logic.ifElse (Lists.null (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeTail (Strings.splitOn (string ".") (var "rawName")))))
         -- Local variable head: the local holds a function value (closure / funcref).
         -- No real call possible without closures; drop args and return a placeholder.
         (right (Lists.concat (list [
           var "droppedArgInstrs",
           list [inject W._Instruction W._Instruction_localGet (var "lname"),
                 inject W._Instruction W._Instruction_drop unit,
                 inject W._Instruction W._Instruction_const $
                   inject W._ConstValue W._ConstValue_i32 (int32 0)]])))
         -- Cross-module or primitive call: push real args, emit call.
         -- The callee's Wasm signature has exactly `n` i32 params where `n` is the
         -- Hydra arity derived from funcSigs. If the call-site provides fewer args
         -- (partial application), we'd need closures — for now, pad with i32.const 0
         -- to match the callee's param count.
         ("mSig" <~ Maps.lookup (var "lname") (var "funcSigs") $
          "callerArgCount" <~ Lists.length (var "args") $
          "calleeParamCount" <~ Maybes.maybe (var "callerArgCount")
            (lambda "sig" $ Lists.length (Pairs.first (var "sig")))
            (var "mSig") $
          "padCount" <~ Math.sub (var "calleeParamCount") (var "callerArgCount") $
          "padInstrs" <~ Lists.concat (Lists.replicate
            (Logic.ifElse (Equality.gt (var "padCount") (int32 0)) (var "padCount") (int32 0))
            (list [inject W._Instruction W._Instruction_const $
              inject W._ConstValue W._ConstValue_i32 (int32 0)])) $
          right (Lists.concat (list [
            var "flatRealArgInstrs",
            var "padInstrs",
            list [inject W._Instruction W._Instruction_call (var "lname")]]))),
     -- Projection applied to args: the first arg is the record being projected from.
     -- Encode just that first arg as the scrutinee, and let encodeProjection emit the
     -- real i32.load. Note: we deliberately skip argInstrs (which would evaluate-and-drop
     -- every arg) because we need the first arg's VALUE on the stack, not just its
     -- side effect. Projection takes only one "real" arg, so extra args would be a
     -- type error at the Hydra level anyway.
     _Term_project>>: lambda "proj" $
       Maybes.cases (Lists.maybeHead (var "args"))
         -- No args: treat as a bare projection function value. Push placeholder.
         (right (list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (lambda "firstArg" $
          "firstArgInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "firstArg") $
          encodeProjection @@ var "cx" @@ var "g" @@ var "fieldOffsets" @@ var "proj" @@ var "firstArgInstrs"),
     -- Case statement applied to args: the first arg is the union value being dispatched
     -- on. Encode just that first arg as the scrutinee, same pattern as _Term_project.
     _Term_cases>>: lambda "cs" $
       Maybes.cases (Lists.maybeHead (var "args"))
         -- No args: treat as a bare cases function value. Push placeholder.
         (right (list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (lambda "firstArg" $
          "firstArgInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "firstArg") $
          encodeCases @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "cs" @@ var "firstArgInstrs"),
     -- Lambda applied to args: drop args, encode the body.
     _Term_lambda>>: lambda "lam" $
       Eithers.bind (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.lambdaBody (var "lam"))
         (lambda "bodyInstrs" $
           right (Lists.concat2 (var "droppedArgInstrs") (var "bodyInstrs")))]


-- =============================================================================
-- Term encoding
-- =============================================================================

-- | Encode a Hydra term as a list of WASM instructions. The stringOffsets argument is
-- a module-level string→data-segment-offset map used to encode _Literal_string values
-- as real memory pointers instead of `i32.const 0` placeholders. The fieldOffsets map
-- is the universe-wide record-type→field-offset table used by encodeProjection. The
-- variantIndexes map is the universe-wide union-type→variant-tag-index table used by
-- `_Term_inject` (tag at construction) and by `encodeCases` (tag dispatch).
encodeTerm :: TTermDefinition (Context -> Graph -> M.Map String Int -> M.Map Name [(Name, Int)] -> M.Map Name [(Name, Int)] -> M.Map String ([W.ValType], [W.ValType]) -> Term -> Either (InContext Error) [W.Instruction])
encodeTerm = def "encodeTerm" $
  "cx" ~> "g" ~> "stringOffsets" ~> "fieldOffsets" ~> "variantIndexes" ~> "funcSigs" ~> lambda "term" $
    cases _Term (var "term") (Just $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "unexpected term variant in WASM encoding") (var "cx"))
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.annotatedTermBody (var "at"),
     _Term_application>>: lambda "app" $
       encodeApplication @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.termApplication (var "app"),
     _Term_cases>>: lambda "_cs" $
       -- A bare cases term is a function value (Union -> a), not a dispatch. Emit a
       -- placeholder i32. Real dispatches happen in encodeApplication's _Term_cases
       -- branch (using the first arg as scrutinee) or in encodeTermDefinition's bare-
       -- eliminator path (using the first param as scrutinee).
       right (list [inject W._Instruction W._Instruction_const $
         inject W._ConstValue W._ConstValue_i32 (int32 0)]),
     _Term_either>>: lambda "e" $
       -- Real either construction. Layout: `[tag, payload_ptr]`, 8 bytes.
       -- Left → tag=0, Right → tag=1. Matches the inject layout (tag + payload)
       -- but uses a fixed {0, 1} tagging instead of a variant-index lookup.
       "eitherTag" <~ Eithers.either_
         (lambda "_lv" $ int32 0)
         (lambda "_rv" $ int32 1)
         (var "e") $
       "innerTerm" <~ Eithers.either_
         (lambda "lv" $ var "lv")
         (lambda "rv" $ var "rv")
         (var "e") $
       "payloadInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "innerTerm") $
       right (Lists.concat (list [
         list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "eitherTag")],
         var "payloadInstrs",
         list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 8),
           inject W._Instruction W._Instruction_call (string "__alloc"),
           inject W._Instruction W._Instruction_localSet (string "__rec_ptr"),
           -- Pop payload (top) into scratch, store at offset 4
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 4,
                 W._MemArg_align>>: int32 2]],
           -- Pop tag into scratch, store at offset 0
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 0,
                 W._MemArg_align>>: int32 2]],
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr")]])),
     _Term_inject>>: lambda "inj" $
       -- Real tagged-union construction: allocate 8 bytes and store [tag, payload_ptr].
       -- Tag is looked up in variantIndexes by (typeName, fieldName). Payload is 0 for
       -- unit-like variants (e.g. True/False in an enum-style union) or the encoded
       -- injected term otherwise. Layout matches what encodeCases expects when it loads
       -- `scrutinee + 0` as the tag and `scrutinee + 4` as the payload.
       "typeName" <~ Core.injectionTypeName (var "inj") $
       "field" <~ Core.injectionField (var "inj") $
       "fieldName" <~ Core.fieldName (var "field") $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Strip.deannotateTerm @@ var "fterm") $
       "isUnit" <~ (cases _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.null (Core.recordFields (var "rt"))]) $
       -- Look up the tag in the universe-wide variant table. Fall back to 0 when the
       -- type or variant is unknown (shouldn't happen for well-formed Hydra terms).
       "mVariants" <~ Maps.lookup (var "typeName") (var "variantIndexes") $
       "tag" <~ Maybes.cases (var "mVariants")
         (int32 0)
         (lambda "pairs" $
           "matching" <~ Lists.filter
             (lambda "p" $ Equality.equal (Pairs.first (var "p")) (var "fieldName"))
             (var "pairs") $
           Maybes.cases (Lists.maybeHead (var "matching"))
             (int32 0)
             (lambda "p" $ Pairs.second (var "p"))) $
       -- Compute payload instructions: `i32.const 0` for unit, else encode the term.
       "payloadInstrs" <<~ Logic.ifElse (var "isUnit")
         (right (list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "fterm") $
       -- Allocate 8 bytes and store [tag, payload] using the same $__rec_ptr /
       -- $__rec_scratch locals that record construction uses. Nested inject + record
       -- construction is safe by the same argument as nested record construction.
       right (Lists.concat (list [
         list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "tag")],
         var "payloadInstrs",
         list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 8),
           inject W._Instruction W._Instruction_call (string "__alloc"),
           inject W._Instruction W._Instruction_localSet (string "__rec_ptr"),
           -- Pop payload (top) into scratch, store at offset 4
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 4,
                 W._MemArg_align>>: int32 2]],
           -- Pop tag (now top) into scratch, store at offset 0
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 0,
                 W._MemArg_align>>: int32 2]],
           -- Push the union-record pointer as the term's single-value result.
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr")]])),
     _Term_lambda>>: lambda "lam" $
       -- Lambdas at the term level: encode the body directly.
       -- Parameters are bound as WASM function locals at the term-definition level
       -- (see encodeTermDefinition's use of extractLambdaParams).
       encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.lambdaBody (var "lam"),
     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       "bindInstrs" <<~ (Eithers.mapList
         (lambda "b" $
           "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.bindingName (var "b"))) $
           "bval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.bindingTerm (var "b")) $
             right (Lists.concat2 (var "bval")
               (list [inject W._Instruction W._Instruction_localSet (var "bname")])))
         (var "bindings")) $
       "bodyInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "body") $
         right (Lists.concat2 (Lists.concat (var "bindInstrs")) (var "bodyInstrs")),
     _Term_list>>: lambda "els" $
       -- Real list construction. Layout: a length-prefixed array of i32 cells,
       -- `[length:i32, elem_0:i32, ..., elem_{N-1}:i32]`, total `(N+1)*4` bytes.
       -- The pointer on the stack points at the length word (offset 0).
       -- Sequence:
       --   1. Evaluate every element (each leaves one i32 on the stack).
       --   2. Allocate `(N+1)*4` bytes; localSet $__rec_ptr.
       --   3. Store the length word at offset 0.
       --   4. For i = N-1 down to 0: pop the top stack value into $__rec_scratch
       --      and store it at offset `(i+1)*4` via $__rec_ptr.
       --   5. Push $__rec_ptr as the term's single-value result.
       -- Nested lists/records are safe because each inner construction leaves exactly
       -- one value on the stack and touches $__rec_ptr / $__rec_scratch only during
       -- its own body. Reuses the same scratch locals as record construction.
       "numElems" <~ Lists.length (var "els") $
       "listSize" <~ Math.mul (Math.add (var "numElems") (int32 1)) (int32 4) $
       "encodedElems" <<~ (Eithers.mapList
         (lambda "el" $
           encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "el")
         (var "els")) $
       "allElemInstrs" <~ Lists.concat (var "encodedElems") $
       "allocInstrs" <~ list [
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "listSize"),
         inject W._Instruction W._Instruction_call (string "__alloc"),
         inject W._Instruction W._Instruction_localSet (string "__rec_ptr")] $
       "lengthStoreInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "numElems"),
         inject W._Instruction W._Instruction_store $
           record W._MemoryInstruction [
             W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
             W._MemoryInstruction_memArg>>: record W._MemArg [
               W._MemArg_offset>>: int32 0,
               W._MemArg_align>>: int32 2]]] $
       -- Reverse index list [N-1, N-2, ..., 0] built the same way as in _Term_record.
       -- Each store pops one value (the last-pushed element first) and writes it to
       -- offset (i+1)*4 so elem_i lands at its declared position.
       "storeInstrs" <~ Lists.concat (Lists.map
         (lambda "i" $
           list [
             inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
             inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_store $
               record W._MemoryInstruction [
                 W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
                 W._MemoryInstruction_memArg>>: record W._MemArg [
                   W._MemArg_offset>>: Math.mul (Math.add (var "i") (int32 1)) (int32 4),
                   W._MemArg_align>>: int32 2]]])
         (Lists.foldl
           (lambda "acc" $ lambda "_e" $ Lists.cons (Lists.length (var "acc")) (var "acc"))
           (list ([] :: [TTerm Int]))
           (var "els"))) $
       "finalInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr")] $
       right (Lists.concat (list [
         var "allElemInstrs",
         var "allocInstrs",
         var "lengthStoreInstrs",
         var "storeInstrs",
         var "finalInstrs"])),
     _Term_literal>>: lambda "lit" $
       -- String literals are the one non-placeholder path: look up the literal's offset
       -- in the module-level stringOffsets map and emit `i32.const <offset>`. All other
       -- literal kinds (i32, bool, placeholder-emitted floats/i64/etc.) go through
       -- encodeLiteral unchanged.
       cases _Literal (var "lit") (Just $
         right (list [encodeLiteral @@ var "lit"])) [
         _Literal_string>>: lambda "s" $
           Maybes.maybe
             -- Unknown string (shouldn't happen if collectStrings was run first):
             -- fall back to the placeholder.
             (right (list [inject W._Instruction W._Instruction_const $
               inject W._ConstValue W._ConstValue_i32 (int32 0)]))
             (lambda "off" $
               right (list [inject W._Instruction W._Instruction_const $
                 inject W._ConstValue W._ConstValue_i32 (var "off")]))
             (Maps.lookup (var "s") (var "stringOffsets"))],
     _Term_map>>: lambda "m" $
       -- Real map construction. Layout: `[length, k_0, v_0, ..., k_{N-1}, v_{N-1}]`,
       -- `(2N+1)*4` bytes. The length word is the number of entries (not the word
       -- count). Keys and values are interleaved and stored at offsets
       -- (2i+1)*4 and (2i+2)*4 respectively. Maps.toList gives entries in
       -- deterministic (sorted) order.
       -- We encode each (k,v) pair into a pair of Instruction lists, then flatten
       -- so the stack has [k_0, v_0, k_1, v_1, ..., k_{N-1}, v_{N-1}] in order.
       "mapEntries" <~ Maps.toList (var "m") $
       "numMapEntries" <~ Lists.length (var "mapEntries") $
       "mapWordCount" <~ Math.add (Math.mul (var "numMapEntries") (int32 2)) (int32 1) $
       "mapSize" <~ Math.mul (var "mapWordCount") (int32 4) $
       "encodedMapKVs" <<~ (Eithers.mapList
         (lambda "kv" $ Eithers.bind
           (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Pairs.first (var "kv"))
           (lambda "kInstrs" $ Eithers.map
             (lambda "vInstrs" $ Lists.concat2 (var "kInstrs") (var "vInstrs"))
             (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Pairs.second (var "kv"))))
         (var "mapEntries")) $
       "allMapKVInstrs" <~ Lists.concat (var "encodedMapKVs") $
       "mapAllocInstrs" <~ list [
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "mapSize"),
         inject W._Instruction W._Instruction_call (string "__alloc"),
         inject W._Instruction W._Instruction_localSet (string "__rec_ptr")] $
       "mapLengthStoreInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "numMapEntries"),
         inject W._Instruction W._Instruction_store $
           record W._MemoryInstruction [
             W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
             W._MemoryInstruction_memArg>>: record W._MemArg [
               W._MemArg_offset>>: int32 0,
               W._MemArg_align>>: int32 2]]] $
       -- The stack order (top to bottom) is v_{N-1}, k_{N-1}, ..., v_0, k_0.
       -- So we store v_{N-1} first at offset (2*(N-1)+2)*4, then k_{N-1} at
       -- (2*(N-1)+1)*4, then v_{N-2} at (2*(N-2)+2)*4, etc. We build a reversed
       -- index list `[2N-1, 2N-2, ..., 1, 0]` (one index per STORED WORD, not per
       -- entry) and store at offset `(j+1)*4` where j is the stored-word index.
       -- This is identical to the list-store pattern but with (2N-1) instead of
       -- (N-1) words.
       "numMapWords" <~ Math.mul (var "numMapEntries") (int32 2) $
       "mapReverseIndices" <~ (Lists.foldl
         (lambda "acc" $ lambda "_i" $ Lists.cons (Lists.length (var "acc")) (var "acc"))
         (list ([] :: [TTerm Int]))
         (Lists.replicate (var "numMapWords") unit)) $
       "mapStoreInstrs" <~ Lists.concat (Lists.map
         (lambda "j" $
           list [
             inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
             inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_store $
               record W._MemoryInstruction [
                 W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
                 W._MemoryInstruction_memArg>>: record W._MemArg [
                   W._MemArg_offset>>: Math.mul (Math.add (var "j") (int32 1)) (int32 4),
                   W._MemArg_align>>: int32 2]]])
         (var "mapReverseIndices")) $
       "mapFinalInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr")] $
       right (Lists.concat (list [
         var "allMapKVInstrs",
         var "mapAllocInstrs",
         var "mapLengthStoreInstrs",
         var "mapStoreInstrs",
         var "mapFinalInstrs"])),
     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         -- Nothing: push 0 (null pointer / tag for None)
         (right (list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 0)]))
         (lambda "val" $
           encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "val"),
     _Term_pair>>: lambda "p" $
       -- Real pair construction. Layout: `[first_ptr, second_ptr]`, 8 bytes.
       -- Encode both components, alloc 8 bytes, store at offsets 0 and 4.
       -- Reuses the same $__rec_ptr / $__rec_scratch scratch locals as record
       -- construction; nested pairs are safe by the same argument.
       "firstInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Pairs.first (var "p")) $
       "secondInstrs" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Pairs.second (var "p")) $
       right (Lists.concat (list [
         var "firstInstrs",
         var "secondInstrs",
         list [
           inject W._Instruction W._Instruction_const $
             inject W._ConstValue W._ConstValue_i32 (int32 8),
           inject W._Instruction W._Instruction_call (string "__alloc"),
           inject W._Instruction W._Instruction_localSet (string "__rec_ptr"),
           -- Pop second (top) into scratch, store at offset 4
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 4,
                 W._MemArg_align>>: int32 2]],
           -- Pop first into scratch, store at offset 0
           inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
           inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
           inject W._Instruction W._Instruction_store $
             record W._MemoryInstruction [
               W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
               W._MemoryInstruction_memArg>>: record W._MemArg [
                 W._MemArg_offset>>: int32 0,
                 W._MemArg_align>>: int32 2]],
           inject W._Instruction W._Instruction_localGet (string "__rec_ptr")]])),
     _Term_project>>: lambda "_proj" $
       -- A bare projection term is a function value (of type Record -> Field), not an
       -- application. We don't have a record pointer to load from, so we emit a single
       -- i32.const 0 placeholder (a "function-index" stand-in). Real loads happen in
       -- encodeApplication's _Term_project case, where the record IS available as the
       -- first argument.
       right (list [inject W._Instruction W._Instruction_const $
         inject W._ConstValue W._ConstValue_i32 (int32 0)]),
     _Term_record>>: lambda "rec" $
       -- Real record construction. Layout: each field is a 4-byte i32, in the order
       -- the fields appear in the record term. Sequence:
       --   1. Evaluate every field value (each leaves one i32 on the stack).
       --   2. Push the record byte size; call $__alloc to get a fresh pointer.
       --   3. Pop the pointer into $__rec_ptr.
       --   4. For each field in REVERSE order (so the stack pops them in field order):
       --        local.set $__rec_scratch  (pops the field value)
       --        local.get $__rec_ptr      (push addr)
       --        local.get $__rec_scratch  (push value)
       --        i32.store offset=<fieldOffset>
       --   5. Push $__rec_ptr as the term's single-value result.
       -- Nested records are safe because each inner record leaves exactly one value on
       -- the stack and touches $__rec_ptr / $__rec_scratch only during its own body.
       "recFields" <~ Core.recordFields (var "rec") $
       "numFields" <~ Lists.length (var "recFields") $
       "recSize" <~ Math.mul (var "numFields") (int32 4) $
       "encodedFields" <<~ (Eithers.mapList
         (lambda "fld" $
           encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs"
             @@ Core.fieldTerm (var "fld"))
         (var "recFields")) $
       "allFieldInstrs" <~ Lists.concat (var "encodedFields") $
       "allocInstrs" <~ list [
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "recSize"),
         inject W._Instruction W._Instruction_call (string "__alloc"),
         inject W._Instruction W._Instruction_localSet (string "__rec_ptr")] $
       -- Build store sequences from the LAST field to the FIRST, because that's the
       -- order the stack pops them. Each store sequence pops one value and reads
       -- $__rec_ptr, so the order within each is: localSet scratch; localGet ptr;
       -- localGet scratch; store offset=i*4.
       "storeInstrs" <~ Lists.concat (Lists.map
         (lambda "i" $
           list [
             inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
             inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_store $
               record W._MemoryInstruction [
                 W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
                 W._MemoryInstruction_memArg>>: record W._MemArg [
                   W._MemArg_offset>>: Math.mul (var "i") (int32 4),
                   W._MemArg_align>>: int32 2]]])
         -- Reverse index list: [N-1, N-2, ..., 0]. Built by a foldl over the field list
         -- that conses `length acc` at each step; since length is always the next index,
         -- the accumulator ends up naturally reversed.
         (Lists.foldl
           (lambda "acc" $ lambda "_f" $ Lists.cons (Lists.length (var "acc")) (var "acc"))
           (list ([] :: [TTerm Int]))
           (var "recFields"))) $
       "finalInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr")] $
       right (Lists.concat (list [
         var "allFieldInstrs",
         var "allocInstrs",
         var "storeInstrs",
         var "finalInstrs"])),
     _Term_set>>: lambda "s" $
       -- Real set construction. Layout identical to list: length-prefixed array
       -- `[length, elem_0, ..., elem_{N-1}]`, `(N+1)*4` bytes. Sets.toList returns
       -- elements in deterministic (sorted) order, so the encoding is stable.
       "setElems" <~ Sets.toList (var "s") $
       "numSetElems" <~ Lists.length (var "setElems") $
       "setSize" <~ Math.mul (Math.add (var "numSetElems") (int32 1)) (int32 4) $
       "encodedSetElems" <<~ (Eithers.mapList
         (lambda "el" $
           encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "el")
         (var "setElems")) $
       "allSetElemInstrs" <~ Lists.concat (var "encodedSetElems") $
       "setAllocInstrs" <~ list [
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "setSize"),
         inject W._Instruction W._Instruction_call (string "__alloc"),
         inject W._Instruction W._Instruction_localSet (string "__rec_ptr")] $
       "setLengthStoreInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
         inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (var "numSetElems"),
         inject W._Instruction W._Instruction_store $
           record W._MemoryInstruction [
             W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
             W._MemoryInstruction_memArg>>: record W._MemArg [
               W._MemArg_offset>>: int32 0,
               W._MemArg_align>>: int32 2]]] $
       "setStoreInstrs" <~ Lists.concat (Lists.map
         (lambda "i" $
           list [
             inject W._Instruction W._Instruction_localSet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
             inject W._Instruction W._Instruction_localGet (string "__rec_scratch"),
             inject W._Instruction W._Instruction_store $
               record W._MemoryInstruction [
                 W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
                 W._MemoryInstruction_memArg>>: record W._MemArg [
                   W._MemArg_offset>>: Math.mul (Math.add (var "i") (int32 1)) (int32 4),
                   W._MemArg_align>>: int32 2]]])
         (Lists.foldl
           (lambda "acc" $ lambda "_e" $ Lists.cons (Lists.length (var "acc")) (var "acc"))
           (list ([] :: [TTerm Int]))
           (var "setElems"))) $
       "setFinalInstrs" <~ list [
         inject W._Instruction W._Instruction_localGet (string "__rec_ptr")] $
       right (Lists.concat (list [
         var "allSetElemInstrs",
         var "setAllocInstrs",
         var "setLengthStoreInstrs",
         var "setStoreInstrs",
         var "setFinalInstrs"])),
     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.typeApplicationTermBody (var "ta"),
     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.typeLambdaBody (var "tl"),
     _Term_unit>>: constant $
       -- Unit: push a placeholder i32 for stack consistency (every term produces 1 value).
       right (list [inject W._Instruction W._Instruction_const $
         inject W._ConstValue W._ConstValue_i32 (int32 0)]),
     _Term_unwrap>>: lambda "_" $
       -- Unwrap is identity in WASM (the wrapper is erased).
       -- Push a placeholder i32 so the term produces exactly 1 value.
       right (list [inject W._Instruction W._Instruction_const $
         inject W._ConstValue W._ConstValue_i32 (int32 0)]),
     _Term_variable>>: lambda "name" $
       "rawName" <~ Core.unName (var "name") $
       "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ var "rawName") $
       -- Unqualified names are local variables → emit local.get.
       -- Qualified names are cross-module function *references* (not calls): we push
       -- an i32 placeholder representing the function index. Actual calls are emitted
       -- from encodeApplication when the function is in head position.
       Logic.ifElse (Lists.null (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeTail (Strings.splitOn (string ".") (var "rawName")))))
         (right (list [inject W._Instruction W._Instruction_localGet (var "lname")]))
         (right (list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (int32 0)])),
     _Term_wrap>>: lambda "wt" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ Core.wrappedTermBody (var "wt")]


-- =============================================================================
-- Elimination encoding
-- =============================================================================

-- | Encode a record projection as WASM instructions.
-- Takes scrutinee instructions to place before the dispatch. The fieldOffsets map is
-- looked up by (typeName, fieldName) to find the record's field offset in bytes. If the
-- type is known, the projection is encoded as `scrutinee; i32.load offset=N` (a real
-- memory read from the record pointer). If the type is unknown (e.g. a projection from
-- a term whose type hasn't been declared as a record), we fall back to the session-8
-- placeholder: drop the scrutinee and push `i32.const 0`.
encodeProjection :: TTermDefinition (Context -> Graph -> M.Map Name [(Name, Int)] -> Projection -> [W.Instruction] -> Either (InContext Error) [W.Instruction])
encodeProjection = def "encodeProjection" $
  "cx" ~> "g" ~> "fieldOffsets" ~> lambda "proj" $ lambda "scrutineeInstrs" $
    "typeName" <~ Core.projectionTypeName (var "proj") $
    "fieldName" <~ Core.projectionField (var "proj") $
    "mFields" <~ Maps.lookup (var "typeName") (var "fieldOffsets") $
    -- Compute the offset if the type is known and the field exists in it. Returns
    -- Maybe Int — Nothing if the type is unknown or the field name doesn't match
    -- any entry in the type's field list.
    "mOffset" <~ (Maybes.cases (var "mFields")
      (nothing :: TTerm (Maybe Int))
      (lambda "pairs" $
        -- Find the first pair (fn, off) where fn == fieldName.
        "matching" <~ Lists.filter
          (lambda "p" $ Equality.equal (Pairs.first (var "p")) (var "fieldName"))
          (var "pairs") $
        Maybes.map (unaryFunction Pairs.second) (Lists.maybeHead (var "matching")))) $
    Maybes.cases (var "mOffset")
      -- Unknown: fall back to placeholder (drop scrutinee if any, push i32.const 0).
      (right (Lists.concat (list [
        var "scrutineeInstrs",
        Logic.ifElse (Lists.null (var "scrutineeInstrs"))
          (list ([] :: [TTerm W.Instruction]))
          (list [inject W._Instruction W._Instruction_drop unit]),
        list [inject W._Instruction W._Instruction_const $
          inject W._ConstValue W._ConstValue_i32 (int32 0)]])))
      -- Known: emit the scrutinee (the record pointer), then i32.load offset=N.
      -- If the scrutinee is empty (shouldn't happen for a real projection, but guard
      -- anyway), synthesize an i32.const 0 placeholder scrutinee so the load has an
      -- address on the stack.
      (lambda "off" $
        right (Lists.concat (list [
          Logic.ifElse (Lists.null (var "scrutineeInstrs"))
            (list [inject W._Instruction W._Instruction_const $
              inject W._ConstValue W._ConstValue_i32 (int32 0)])
            (var "scrutineeInstrs"),
          list [inject W._Instruction W._Instruction_load $
            record W._MemoryInstruction [
              W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
              W._MemoryInstruction_memArg>>: record W._MemArg [
                W._MemArg_offset>>: var "off",
                W._MemArg_align>>: int32 2]]]])))

-- | Encode a case statement (union elimination) as WASM instructions.
-- Takes scrutinee instructions to place before the dispatch.
encodeCases :: TTermDefinition (Context -> Graph -> M.Map String Int -> M.Map Name [(Name, Int)] -> M.Map Name [(Name, Int)] -> M.Map String ([W.ValType], [W.ValType]) -> CaseStatement -> [W.Instruction] -> Either (InContext Error) [W.Instruction])
encodeCases = def "encodeCases" $
  "cx" ~> "g" ~> "stringOffsets" ~> "fieldOffsets" ~> "variantIndexes" ~> "funcSigs" ~> lambda "cs" $ lambda "scrutineeInstrsRaw" $
    "tname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Names.localNameOf @@ Core.caseStatementTypeName (var "cs"))) $
    "caseFields" <~ Core.caseStatementCases (var "cs") $
    -- Ensure the scrutinee pushes exactly one i32 onto the stack.
    -- If the caller passed no instructions, synthesize a placeholder pointer.
    "scrutineeInstrs" <~ Logic.ifElse (Lists.null (var "scrutineeInstrsRaw"))
      (list [inject W._Instruction W._Instruction_const $
        inject W._ConstValue W._ConstValue_i32 (int32 0)])
      (var "scrutineeInstrsRaw") $
    -- Scrutinee prologue: evaluate the scrutinee pointer, save to $__rec_ptr (reused
    -- as a scratch local — nested cases/records overwrite it within their own body
    -- and restore nothing, which is safe because the outer case has already bound $v
    -- and loaded its tag before running any arm body). Then:
    --   1. Load payload from offset 4 and bind to $v (the fresh case variable).
    --   2. Load tag from offset 0 and leave it on the stack for the br_table dispatch.
    "prologue" <~ Lists.concat (list [
      var "scrutineeInstrs",
      list [
        inject W._Instruction W._Instruction_localSet (string "__rec_ptr"),
        -- Load payload and bind to $v
        inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
        inject W._Instruction W._Instruction_load $
          record W._MemoryInstruction [
            W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
            W._MemoryInstruction_memArg>>: record W._MemArg [
              W._MemArg_offset>>: int32 4,
              W._MemArg_align>>: int32 2]],
        inject W._Instruction W._Instruction_localSet (string "v"),
        -- Load tag (selector for br_table)
        inject W._Instruction W._Instruction_localGet (string "__rec_ptr"),
        inject W._Instruction W._Instruction_load $
          record W._MemoryInstruction [
            W._MemoryInstruction_type>>: inject W._ValType W._ValType_i32 unit,
            W._MemoryInstruction_memArg>>: record W._MemArg [
              W._MemArg_offset>>: int32 0,
              W._MemArg_align>>: int32 2]]]]) $
    -- Encode each case branch body. Each arm is still synthesized as
    -- `apply (cfterm) (termVariable "v")` — encodeTerm will resolve `v` to `local.get $v`,
    -- which is now initialized to the loaded payload before any arm runs.
    "arms" <<~ (Eithers.mapList
      (lambda "cf" $
        "cfname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldName (var "cf"))) $
        "cfterm" <~ Core.fieldTerm (var "cf") $
        "armBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@
          (Core.termApplication (Core.application (var "cfterm")
            (Core.termVariable (wrap _Name (string "v")))))) $
          right (pair (var "cfname") (var "armBody")))
      (var "caseFields")) $
    -- Block-based union dispatch: nested empty dispatch blocks, innermost holds the
    -- prologue + br_table, outer blocks wrap each arm body.
    "armLabels" <~ Lists.map (unaryFunction Pairs.first) (var "arms") $
    "endLabel" <~ (Strings.cat2 (string "end_") (var "tname")) $
    "defaultLabel" <~ Maybes.fromMaybe (var "endLabel") (Lists.maybeLast (var "armLabels")) $
    "innerDispatch" <~ Lists.concat2
      (var "prologue")
      (list [inject W._Instruction W._Instruction_brTable $
        record W._BrTableArgs [
          W._BrTableArgs_labels>>: var "armLabels",
          W._BrTableArgs_default>>: var "defaultLabel"]]) $
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
      -- Short-circuit empty-cases: with zero arms, the prologue still runs (reading
      -- the scrutinee for side effects and dropping the tag+payload), then we push
      -- an i32.const 0 placeholder as the block's result.
      Logic.ifElse (Lists.null (var "arms"))
        (right (Lists.concat (list [
          var "scrutineeInstrs",
          list [inject W._Instruction W._Instruction_drop unit],
          list [inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (int32 0)]])))
        -- Wrap everything in the outermost end block with result type
        (right (list [inject W._Instruction W._Instruction_block $
          record W._BlockInstruction [
            W._BlockInstruction_label>>: just (var "endLabel"),
            W._BlockInstruction_blockType>>: inject W._BlockType W._BlockType_value (inject W._ValType W._ValType_i32 unit),
            W._BlockInstruction_body>>: var "dispatch"]]))


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
      _Term_lambda>>: lambda "lam" $
        "paramName" <~ Core.lambdaParameter (var "lam") $
        "body" <~ Core.lambdaBody (var "lam") $
        "inner" <~ (extractLambdaParams @@ var "body") $
          pair
            (Lists.cons (var "paramName") (Pairs.first (var "inner")))
            (Pairs.second (var "inner")),
      -- Bare eliminations: generate a synthetic parameter name.
      -- The elimination itself becomes the body (applied to the synthetic param).
      _Term_cases>>: lambda "_cs" $
        pair (list [wrap _Name (string "arg_0")]) (var "term"),
      _Term_project>>: lambda "_proj" $
        pair (list [wrap _Name (string "arg_0")]) (var "term"),
      _Term_unwrap>>: lambda "_name" $
        pair (list [wrap _Name (string "arg_0")]) (var "term"),
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

-- | Clamp a list of Wasm value types to all-i32. The coder's function bodies still
-- assume the 1-i32-per-term stack invariant (sessions 8-15), so non-i32 types like
-- f64 or i64 in a primitive's signature would violate that invariant at call sites.
-- Clamping to i32 universally keeps the body encoding valid while letting the signature
-- have the right arity. Runtime semantic correctness for non-i32 primitives (floats,
-- i64 integers) is deferred.
clampValTypesToI32 :: TTermDefinition ([W.ValType] -> [W.ValType])
clampValTypesToI32 = def "clampValTypesToI32" $
  lambda "vts" $
    Lists.map (lambda "_vt" $ inject W._ValType W._ValType_i32 unit) (var "vts")

-- | Extract a complete function signature (params + results) from a Hydra type,
-- clamping every Wasm value type to i32. Returns a pair `(paramTypes, resultTypes)`
-- suitable for direct use in a Wasm TypeUse. Non-function types have zero params
-- and a single i32 result (constants).
extractSignature :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) ([W.ValType], [W.ValType]))
extractSignature = def "extractSignature" $
  "cx" ~> "g" ~> lambda "t" $
    "params" <<~ (extractParamTypes @@ var "cx" @@ var "g" @@ var "t") $
    "results" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "t") $
      right (pair (clampValTypesToI32 @@ var "params") (clampValTypesToI32 @@ var "results"))

-- | Build a universe-wide map from snake-cased function names to Wasm signatures.
-- Walks three sources: (1) the graph's primitives (hydra.lib.*), (2) the graph's
-- boundTypes (all kernel cross-module definitions), and (3) the current module's
-- own term definitions (for internal consistency). Each entry's Hydra type scheme
-- is stripped of its Forall and passed through extractSignature. Any entry whose
-- type extraction fails is silently dropped — the call site will fall back to
-- the default (param i32) (result i32) signature.
--
-- Keys are snake-cased to match the form used by Formatting.convertCaseCamelToLowerSnake
-- at call and import emission sites. If two distinct Hydra names collide to the same
-- snake form, the last one wins (deterministic by Maps.toList order).
buildFunctionSignatures :: TTermDefinition (Context -> Graph -> [TermDefinition] -> M.Map String ([W.ValType], [W.ValType]))
buildFunctionSignatures = def "buildFunctionSignatures" $
  "cx" ~> "g" ~> lambda "termDefs" $
    -- Helper: given a (Name, TypeScheme) pair, produce a (snakeName, signature) pair
    -- or Nothing on failure.
    "toSigEntry" <~ (lambda "nameAndScheme" $
      "nm" <~ Pairs.first (var "nameAndScheme") $
      "ts" <~ Pairs.second (var "nameAndScheme") $
      "snakeName" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "nm")) $
      "sigEither" <~ (extractSignature @@ var "cx" @@ var "g" @@ Core.typeSchemeType (var "ts")) $
      Eithers.either_
        (lambda "_err" $ nothing)
        (lambda "sig" $ just (pair (var "snakeName") (var "sig")))
        (var "sigEither")) $
    -- Primitives: walk graph.primitives -> Map Name Primitive, extract type from each.
    "primEntries" <~ Maybes.cat (Lists.map
      (lambda "kv" $ var "toSigEntry" @@ pair (Pairs.first (var "kv")) (DslGraph.primitiveType (Pairs.second (var "kv"))))
      (Maps.toList (DslGraph.graphPrimitives (var "g")))) $
    -- Cross-module bound term types: walk graph.boundTypes -> Map Name TypeScheme.
    "boundEntries" <~ Maybes.cat (Lists.map
      (lambda "kv" $ var "toSigEntry" @@ var "kv")
      (Maps.toList (DslGraph.graphBoundTypes (var "g")))) $
    -- Current module's own term defs: use their optional TypeScheme field directly.
    "localEntries" <~ Maybes.cat (Lists.map
      (lambda "td" $
        Maybes.bind (Packaging.termDefinitionType (var "td"))
          (lambda "ts" $ var "toSigEntry" @@ pair (Packaging.termDefinitionName (var "td")) (var "ts")))
      (var "termDefs")) $
    Maps.fromList (Lists.concat (list [var "primEntries", var "boundEntries", var "localEntries"]))

-- | Encode a Hydra term definition as a WASM function. The stringOffsets map is threaded
-- down to encodeTerm so that _Literal_string values can be resolved to real memory offsets.
-- The fieldOffsets map is threaded down to encodeProjection so that record projection emits
-- a real `i32.load offset=N` instead of the placeholder. The variantIndexes map is threaded
-- down to encodeTerm (for _Term_inject tags) and encodeCases (for br_table tag dispatch).
encodeTermDefinition :: TTermDefinition (Context -> Graph -> M.Map String Int -> M.Map Name [(Name, Int)] -> M.Map Name [(Name, Int)] -> M.Map String ([W.ValType], [W.ValType]) -> TermDefinition -> Either (InContext Error) W.ModuleField)
encodeTermDefinition = def "encodeTermDefinition" $
  "cx" ~> "g" ~> "stringOffsets" ~> "fieldOffsets" ~> "variantIndexes" ~> "funcSigs" ~> lambda "tdef" $
    "name" <~ Packaging.termDefinitionName (var "tdef") $
    "term" <~ Packaging.termDefinitionTerm (var "tdef") $
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "name")) $
    "typ" <~ Maybes.maybe
      (Core.typeUnit)
      (unaryFunction Core.typeSchemeType)
      (Packaging.termDefinitionType (var "tdef")) $
    -- Extract lambda parameters and inner body
    "extracted" <~ (extractLambdaParams @@ var "term") $
    "paramNames" <~ Pairs.first (var "extracted") $
    "innerBody" <~ Pairs.second (var "extracted") $
    -- Real n-ary calling convention: one Wasm param per Hydra lambda param, all i32.
    -- Each param is named after the Hydra param (snake-cased).
    "paramNameStrs" <~ Lists.map
      (lambda "pn" $ Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "pn"))
      (var "paramNames") $
    "wasmParams" <~ Lists.map
      (lambda "pn" $ record W._Param [
        W._Param_name>>: just (var "pn"),
        W._Param_type>>: inject W._ValType W._ValType_i32 unit])
      (var "paramNameStrs") $
    -- No initialization prologue needed: params are real Wasm params, directly accessible
    -- via local.get by their names. No copying from a synthetic $arg_0.
    "initPrologue" <~ (list ([] :: [TTerm W.Instruction])) $
    -- Every function returns a single i32 (the body encoding still produces one i32 per term).
    "resultTypes" <~ list [inject W._ValType W._ValType_i32 unit] $
    -- Encode body: for bare eliminations (detected by extractLambdaParams producing synthetic
    -- param "arg_0"), pass local.get of the first Hydra param as the scrutinee.
    "dBody" <~ (Strip.deannotateTerm @@ var "innerBody") $
    "scrutineeInstrs" <~ Maybes.cases (Lists.maybeHead (var "paramNameStrs"))
      (list ([] :: [TTerm W.Instruction]))
      (lambda "p0" $ list [inject W._Instruction W._Instruction_localGet (var "p0")]) $
    "rawBodyInstrs" <<~ (cases _Term (var "dBody") (Just $
        encodeTerm @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "innerBody")
      [_Term_project>>: lambda "proj" $
         encodeProjection @@ var "cx" @@ var "g" @@ var "fieldOffsets" @@ var "proj" @@ var "scrutineeInstrs",
       _Term_cases>>: lambda "cs" $
         encodeCases @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs" @@ var "cs" @@ var "scrutineeInstrs",
       _Term_unwrap>>: lambda "_name" $
         -- Unwrap is identity; keep the 1-value invariant.
         right (list [inject W._Instruction W._Instruction_const $
           inject W._ConstValue W._ConstValue_i32 (int32 0)])]) $
    "bodyInstrs" <~ Lists.concat2 (var "initPrologue") (var "rawBodyInstrs") $
    -- Collect all local variable names referenced in the body instructions
    "referencedLocals" <~ (collectInstructionLocals @@ var "bodyInstrs") $
    -- Union referenced locals with all Hydra param names (so param locals always exist,
    -- even if the body never reads them), then subtract the real WASM params (which are
    -- already declared in the function signature, not as locals).
    "allLocalNames" <~ Sets.toList (Sets.difference
      (var "referencedLocals")
      (Sets.fromList (var "paramNameStrs"))) $
    "wasmLocals" <~ Lists.map
      (lambda "ln" $
        record W._FuncLocal [
          W._FuncLocal_name>>: just (var "ln"),
          -- Default to i32 for locals (all compound types are pointers)
          W._FuncLocal_type>>: inject W._ValType W._ValType_i32 unit])
      (var "allLocalNames") $
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
-- String data segment support
-- =============================================================================

-- | Collect every distinct string literal reachable from a list of term definitions.
-- Uses Rewriting.foldOverTerm in pre-order to walk every subterm and accumulate
-- _Literal_string values into a Set, then returns the Set as a sorted list so that
-- offset assignments are deterministic across regenerations.
collectStrings :: TTermDefinition ([TermDefinition] -> [String])
collectStrings = def "collectStrings" $
  lambda "termDefs" $
    "collectOne" <~ (lambda "acc" $ lambda "t" $
      cases _Term (var "t") (Just $ var "acc") [
        _Term_literal>>: lambda "lit" $
          cases _Literal (var "lit") (Just $ var "acc") [
            _Literal_string>>: lambda "s" $ Sets.insert (var "s") (var "acc")]]) $
    "allStrings" <~ Lists.foldl
      (lambda "acc" $ lambda "td" $
        Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@
          (var "collectOne") @@ (var "acc") @@ (Packaging.termDefinitionTerm (var "td")))
      (Sets.empty :: TTerm (S.Set String))
      (var "termDefs") $
    Sets.toList (var "allStrings")

-- | Build a module-level field-offset table from a Graph. For each record type defined
-- anywhere in the universe (i.e. every entry in `graphSchemaTypes`), walks its `TypeScheme`
-- (stripping `Annotated`, `Forall`, and `TypeLambda` layers) and, if the underlying type is
-- `_Type_record [FieldType]`, records a list of (fieldName, byteOffset) pairs in declaration
-- order (offsets 0, 4, 8, ...). Non-record types contribute nothing.
--
-- The resulting `M.Map Name [(Name, Int)]` is consumed by `encodeProjection` to replace
-- placeholder projection output with a real `i32.load offset=N` at `scrutinee + fieldOffset`.
-- Using the full graph (not just the current module's type defs) is essential because
-- most record projections reference types defined in other modules — e.g. every
-- projection of a `hydra.core.Record` happens in modules that don't themselves define
-- that type.
buildFieldOffsets :: TTermDefinition (Graph -> M.Map Name [(Name, Int)])
buildFieldOffsets = def "buildFieldOffsets" $
  lambda "g" $
    -- Return Just [FieldType] if the stripped type is a record; Nothing otherwise.
    -- Handles two layers of Forall + one Annotated wrapper, which covers the kernel.
    "recordFieldsOf" <~ (lambda "t" $
      "stripped" <~ (Strip.deannotateType @@ var "t") $
      cases _Type (var "stripped") (Just $ (nothing :: TTerm (Maybe [FieldType]))) [
        _Type_record>>: lambda "fts" $ just (var "fts"),
        _Type_forall>>: lambda "fa" $
          -- A Forall over a record: recurse one level.
          "innerStripped" <~ (Strip.deannotateType @@ Core.forallTypeBody (var "fa")) $
          cases _Type (var "innerStripped") (Just $ (nothing :: TTerm (Maybe [FieldType]))) [
            _Type_record>>: lambda "fts" $ just (var "fts")]]) $
    "entryFor" <~ (lambda "nameSchemePair" $
      "tname" <~ Pairs.first (var "nameSchemePair") $
      "tscheme" <~ Pairs.second (var "nameSchemePair") $
      "tbody" <~ Core.typeSchemeType (var "tscheme") $
      "mfields" <~ (var "recordFieldsOf" @@ var "tbody") $
      Maybes.cases (var "mfields")
        (nothing :: TTerm (Maybe (Name, [(Name, Int)])))
        (lambda "fts" $
          "namedOffsets" <~ (Lists.map
            (lambda "p" $
              "i" <~ Pairs.first (var "p") $
              "ft" <~ Pairs.second (var "p") $
                pair (Core.fieldTypeName (var "ft")) (Math.mul (var "i") (int32 4)))
            -- Zip [0..N-1] with the field type list.
            (Lists.zip
              (Lists.foldl
                (lambda "acc" $ lambda "_f" $ Lists.concat2 (var "acc") (list [Lists.length (var "acc")]))
                (list ([] :: [TTerm Int]))
                (var "fts"))
              (var "fts"))) $
          just (pair (var "tname") (var "namedOffsets")))) $
    "schemaTypesList" <~ (Maps.toList (DslGraph.graphSchemaTypes (var "g"))) $
    "entries" <~ (Maybes.cat (Lists.map (var "entryFor") (var "schemaTypesList"))) $
    Maps.fromList (var "entries")

-- | Build a universe-wide variant-tag table from a Graph. For each union type defined
-- anywhere in the graph's schemaTypes, walks the stripped `TypeScheme` and, if the
-- underlying type is `_Type_union [FieldType]`, records a list of (variantName, tagIndex)
-- pairs in declaration order (indexes 0, 1, 2, ...). Non-union types contribute nothing.
--
-- The resulting `M.Map Name [(Name, Int)]` is consumed by `encodeTerm`'s `_Term_inject`
-- case to look up the tag index to store at offset 0 of the allocated union record. The
-- indexes here are raw integers (not byte offsets), unlike `buildFieldOffsets`.
buildVariantIndexes :: TTermDefinition (Graph -> M.Map Name [(Name, Int)])
buildVariantIndexes = def "buildVariantIndexes" $
  lambda "g" $
    -- Return Just [FieldType] if the stripped type is a union; Nothing otherwise.
    "unionFieldsOf" <~ (lambda "t" $
      "stripped" <~ (Strip.deannotateType @@ var "t") $
      cases _Type (var "stripped") (Just $ (nothing :: TTerm (Maybe [FieldType]))) [
        _Type_union>>: lambda "fts" $ just (var "fts"),
        _Type_forall>>: lambda "fa" $
          "innerStripped" <~ (Strip.deannotateType @@ Core.forallTypeBody (var "fa")) $
          cases _Type (var "innerStripped") (Just $ (nothing :: TTerm (Maybe [FieldType]))) [
            _Type_union>>: lambda "fts" $ just (var "fts")]]) $
    "entryFor" <~ (lambda "nameSchemePair" $
      "tname" <~ Pairs.first (var "nameSchemePair") $
      "tscheme" <~ Pairs.second (var "nameSchemePair") $
      "tbody" <~ Core.typeSchemeType (var "tscheme") $
      "mfields" <~ (var "unionFieldsOf" @@ var "tbody") $
      Maybes.cases (var "mfields")
        (nothing :: TTerm (Maybe (Name, [(Name, Int)])))
        (lambda "fts" $
          "namedIndexes" <~ (Lists.map
            (lambda "p" $
              "i" <~ Pairs.first (var "p") $
              "ft" <~ Pairs.second (var "p") $
                pair (Core.fieldTypeName (var "ft")) (var "i"))
            -- Zip [0..N-1] with the field type list.
            (Lists.zip
              (Lists.foldl
                (lambda "acc" $ lambda "_f" $ Lists.concat2 (var "acc") (list [Lists.length (var "acc")]))
                (list ([] :: [TTerm Int]))
                (var "fts"))
              (var "fts"))) $
          just (pair (var "tname") (var "namedIndexes")))) $
    "schemaTypesList" <~ (Maps.toList (DslGraph.graphSchemaTypes (var "g"))) $
    "entries" <~ (Maybes.cat (Lists.map (var "entryFor") (var "schemaTypesList"))) $
    Maps.fromList (var "entries")

-- | Assign a byte offset to each distinct string. Offsets start at 1024 (reserving the
-- low 1KB for a null-pointer region) and grow by 4 + UTF-8 byte count per entry, to
-- match the length-prefixed layout used by stringDataSegment. Each length-prefix is
-- 4 bytes (little-endian i32 length). Returns a pair (offsetMap, bumpStart) where
-- bumpStart is the next 16-byte-aligned offset past the end of the string segment;
-- it's used to initialize the runtime bump allocator.
buildStringOffsets :: TTermDefinition ([String] -> (M.Map String Int, Int))
buildStringOffsets = def "buildStringOffsets" $
  lambda "strs" $
    "step" <~ (lambda "acc" $ lambda "s" $
      "m" <~ Pairs.first (var "acc") $
      "off" <~ Pairs.second (var "acc") $
      "len" <~ Strings.length (var "s") $
        pair
          (Maps.insert (var "s") (var "off") (var "m"))
          (Math.add (var "off") (Math.add (int32 4) (var "len")))) $
    "final" <~ (Lists.foldl (var "step")
      (pair (Maps.empty :: TTerm (M.Map String Int)) (int32 1024))
      (var "strs")) $
    "rawEnd" <~ Pairs.second (var "final") $
    -- Round up to the next 16-byte boundary so the bump pointer starts aligned.
    "aligned" <~ Math.mul (Maybes.fromMaybe (int32 0) (Math.maybeDiv (Math.add (var "rawEnd") (int32 15)) (int32 16))) (int32 16) $
    pair (Pairs.first (var "final")) (var "aligned")

-- | Emit a single byte as a two-character lowercase hex escape prefixed with backslash,
-- e.g. 65 -> "\41". Inputs outside 0..255 (e.g. non-ASCII code points from Strings.toList)
-- are masked to the low 8 bits, so the output is always well-formed WAT. This is a lossy
-- fallback for non-ASCII kernel strings; proper UTF-8 byte encoding is future work.
hexEscapeString :: TTermDefinition (Int -> String)
hexEscapeString = def "hexEscapeString" $
  lambda "b" $
    "byte" <~ (Maybes.fromMaybe (int32 0) (Math.maybeMod (var "b") (int32 256))) $
    "digitToHex" <~ (lambda "d" $
      Logic.ifElse (Equality.lt (var "d") (int32 10))
        -- '0' = 48
        (Strings.fromList (list [Math.add (var "d") (int32 48)]))
        -- 'a' - 10 = 87
        (Strings.fromList (list [Math.add (var "d") (int32 87)]))) $
    Strings.cat (list [
      string "\\",
      var "digitToHex" @@ (Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "byte") (int32 16))),
      var "digitToHex" @@ (Maybes.fromMaybe (int32 0) (Math.maybeMod (var "byte") (int32 16)))])

-- | Build the (data ...) module field for a string-offset map. The data segment contains,
-- for each string s with offset off, a 4-byte little-endian length prefix followed by the
-- UTF-8 bytes of s. Strings are emitted in offset order, so consecutive entries pack
-- contiguously starting at offset 1024. The segment is emitted as a single hex-escaped
-- byte string (two hex chars per byte) to avoid any edge cases with quotes, backslashes,
-- newlines, or non-ASCII bytes inside WAT string literals.
--
-- If the map is empty, emits a zero-length data segment at offset 1024 (benign).
stringDataSegment :: TTermDefinition (M.Map String Int -> W.ModuleField)
stringDataSegment = def "stringDataSegment" $
  lambda "offsets" $
    -- Sort (string, offset) pairs by offset so the data segment packs contiguously
    "entries" <~ Lists.sortOn (unaryFunction Pairs.second) (Maps.toList (var "offsets")) $
    -- For each string: emit 4 length-prefix bytes (little-endian) followed by content bytes,
    -- every byte as a hex escape.
    "bytesForEntry" <~ (lambda "entry" $
      "s" <~ Pairs.first (var "entry") $
      "len" <~ Strings.length (var "s") $
      "lenBytes" <~ list [
        Maybes.fromMaybe (int32 0) (Math.maybeMod (var "len") (int32 256)),
        Maybes.fromMaybe (int32 0) (Math.maybeMod (Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "len") (int32 256))) (int32 256)),
        Maybes.fromMaybe (int32 0) (Math.maybeMod (Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "len") (int32 65536))) (int32 256)),
        Maybes.fromMaybe (int32 0) (Math.maybeMod (Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "len") (int32 16777216))) (int32 256))] $
      "contentBytes" <~ Strings.toList (var "s") $
      "allBytes" <~ Lists.concat2 (var "lenBytes") (var "contentBytes") $
        Strings.cat (Lists.map (lambda "b" $ hexEscapeString @@ var "b") (var "allBytes"))) $
    "allHex" <~ Strings.cat (Lists.map (var "bytesForEntry") (var "entries")) $
    inject W._ModuleField W._ModuleField_data $
      record W._DataSegment [
        W._DataSegment_name>>: nothing,
        W._DataSegment_mode>>: inject W._DataMode W._DataMode_active (list [
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (int32 1024)]),
        W._DataSegment_bytes>>: var "allHex"]


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
    -- Build the module-level string→offset map once, before any term encoding runs,
    -- by walking every term definition and collecting _Literal_string values.
    "stringList" <~ (collectStrings @@ var "termDefs") $
    "stringOffsetsAndEnd" <~ (buildStringOffsets @@ var "stringList") $
    "stringOffsets" <~ Pairs.first (var "stringOffsetsAndEnd") $
    "stringEnd" <~ Pairs.second (var "stringOffsetsAndEnd") $
    "dataField" <~ (stringDataSegment @@ var "stringOffsets") $
    -- Build the universe-wide record-type→field-offset and union-type→variant-tag-index
    -- tables once, before any term encoding runs, by walking every type in the graph's
    -- schemaTypes. Using the full graph (not just this module's type defs) is essential
    -- because most projections and injections reference types defined in other modules.
    "fieldOffsets" <~ (buildFieldOffsets @@ var "g") $
    "variantIndexes" <~ (buildVariantIndexes @@ var "g") $
    "funcSigs" <~ (buildFunctionSignatures @@ var "cx" @@ var "g" @@ var "termDefs") $
    "typeFields" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "termFields" <<~ (Eithers.mapList (encodeTermDefinition @@ var "cx" @@ var "g" @@ var "stringOffsets" @@ var "fieldOffsets" @@ var "variantIndexes" @@ var "funcSigs") (var "termDefs")) $
    "allFields" <~ Lists.concat2 (Lists.concat (var "typeFields")) (var "termFields") $
    -- The bump allocator global: starts just past the string data segment, grows upward.
    -- Records and other heap-allocated compound values occupy memory from stringEnd
    -- onward. Must be mutable so $__alloc can update it.
    "bumpGlobal" <~ (inject W._ModuleField W._ModuleField_global $
      record W._GlobalDef [
        W._GlobalDef_name>>: just (string "__bump_ptr"),
        W._GlobalDef_type>>: record W._GlobalType [
          W._GlobalType_valType>>: inject W._ValType W._ValType_i32 unit,
          W._GlobalType_mutable>>: boolean True],
        W._GlobalDef_init>>: list [
          inject W._Instruction W._Instruction_const $
            inject W._ConstValue W._ConstValue_i32 (var "stringEnd")]]) $
    -- The bump allocator helper: (func $__alloc (param $sz i32) (result i32)). Reads
    -- the current bump pointer, advances it by $sz, returns the original value.
    --   global.get $__bump_ptr   ;; result = old ptr
    --   global.get $__bump_ptr
    --   local.get $sz
    --   i32.add
    --   global.set $__bump_ptr    ;; advance
    "allocFunc" <~ (inject W._ModuleField W._ModuleField_func $
      record W._Func [
        W._Func_name>>: just (string "__alloc"),
        W._Func_typeUse>>: record W._TypeUse [
          W._TypeUse_index>>: nothing,
          W._TypeUse_params>>: list [record W._Param [
            W._Param_name>>: just (string "sz"),
            W._Param_type>>: inject W._ValType W._ValType_i32 unit]],
          W._TypeUse_results>>: list [inject W._ValType W._ValType_i32 unit]],
        W._Func_locals>>: list ([] :: [TTerm W.FuncLocal]),
        W._Func_body>>: list [
          inject W._Instruction W._Instruction_globalGet (string "__bump_ptr"),
          inject W._Instruction W._Instruction_globalGet (string "__bump_ptr"),
          inject W._Instruction W._Instruction_localGet (string "sz"),
          inject W._Instruction W._Instruction_binop $
            record W._NumericOp [
              W._NumericOp_type>>: inject W._ValType W._ValType_i32 unit,
              W._NumericOp_name>>: string "add"],
          inject W._Instruction W._Instruction_globalSet (string "__bump_ptr")]]) $
    -- Add a memory definition (2 pages = 128KB): room for the string data segment,
    -- a bump allocator arena, and the stack for nested records.
    "memField" <~ (inject W._ModuleField W._ModuleField_memory $
      record W._MemoryDef [
        W._MemoryDef_name>>: just (string "memory"),
        W._MemoryDef_limits>>: record W._Limits [
          W._Limits_min>>: int32 2,
          W._Limits_max>>: nothing]]) $
    -- Add memory export
    "memExport" <~ (inject W._ModuleField W._ModuleField_export $
      record W._ExportDef [
        W._ExportDef_name>>: string "memory",
        W._ExportDef_desc>>: inject W._ExportDesc W._ExportDesc_memory (string "memory")]) $
    -- Export the bump pointer global so the JS host can observe allocations.
    "bumpExport" <~ (inject W._ModuleField W._ModuleField_export $
      record W._ExportDef [
        W._ExportDef_name>>: string "__bump_ptr",
        W._ExportDef_desc>>: inject W._ExportDesc W._ExportDesc_global (string "__bump_ptr")]) $
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
    -- Local function names (defined in this module), plus the runtime helper __alloc
    -- (which is emitted by the coder itself, not imported).
    "localFuncNames" <~ Sets.insert (string "__alloc") (Sets.fromList (Lists.map
      (lambda "td" $
        Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Packaging.termDefinitionName (var "td")))
      (var "termDefs"))) $
    -- External call targets = all calls minus local functions (including __alloc)
    "externalCalls" <~ Sets.toList (Sets.difference (var "allCallTargets") (var "localFuncNames")) $
    -- Generate import declarations for external functions, using the signature from
    -- funcSigs when available, falling back to the old (param i32) (result i32) shape.
    "defaultSig" <~ pair
      (list [inject W._ValType W._ValType_i32 unit])
      (list [inject W._ValType W._ValType_i32 unit]) $
    "importFields" <~ Lists.map
      (lambda "fname" $
        "parts" <~ (Strings.splitOn (string ".") (var "fname")) $
        "modName" <~ Strings.intercalate (string ".") (Lists.reverse (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeTail (Lists.reverse (var "parts"))))) $
        "sig" <~ Maybes.fromMaybe (var "defaultSig") (Maps.lookup (var "fname") (var "funcSigs")) $
        "sigParams" <~ Pairs.first (var "sig") $
        "sigResults" <~ Pairs.second (var "sig") $
        "wasmImportParams" <~ Lists.map
          (lambda "vt" $ record W._Param [
            W._Param_name>>: nothing,
            W._Param_type>>: var "vt"])
          (var "sigParams") $
          inject W._ModuleField W._ModuleField_import $
            record W._ImportDef [
              W._ImportDef_module>>: var "modName",
              W._ImportDef_name>>: var "fname",
              W._ImportDef_desc>>: inject W._ImportDesc W._ImportDesc_func $
                record W._ImportFunc [
                  W._ImportFunc_name>>: just (var "fname"),
                  W._ImportFunc_typeUse>>: record W._TypeUse [
                    W._TypeUse_index>>: nothing,
                    W._TypeUse_params>>: var "wasmImportParams",
                    W._TypeUse_results>>: var "sigResults"]]])
      (var "externalCalls") $
    "wasmMod" <~ (record W._Module [
      W._Module_name>>: nothing,
      W._Module_fields>>: Lists.concat (list [
        var "importFields",
        list [var "memField", var "memExport", var "dataField",
              var "bumpGlobal", var "bumpExport", var "allocFunc"],
        var "funcExports",
        var "allFields"])]) $
    "code" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (WasmSerdeSource.moduleToExpr @@ var "wasmMod"))) $
    "filePath" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "wat") @@ (Packaging.moduleNamespace (var "mod"))) $
      right (Maps.singleton (var "filePath") (var "code"))
