-- | Python code generator in Hydra DSL.
-- This module provides DSL versions of all Python code generation functions,
-- including the main entry points (moduleToPython, encodePythonModule).

module Hydra.Ext.Sources.Python.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Ext.Python.Helpers as PyHelpers
import qualified Hydra.Ext.Sources.Python.Syntax as PySyntax
import qualified Hydra.Ext.Sources.Python.Helpers as PyHelpersSource
import qualified Hydra.Ext.Sources.Python.Serde as PySerde
import qualified Hydra.Ext.Sources.Python.Names as PyNames
import qualified Hydra.Ext.Sources.Python.Utils as PyUtils
import qualified Hydra.Ext.Dsl.Python.Syntax as PyDsl
import qualified Hydra.Sources.CoderUtils as CoderUtils
import qualified Hydra.Typing as HydraTyping


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.python.coder"

module_ :: Module
module_ = Module ns elements
    [PyUtils.ns, PyNames.ns, PySerde.ns, Serialization.ns, Schemas.ns, Rewriting.ns, ShowCore.ns, CoderUtils.ns, Reduction.ns, Sorting.ns, Names.ns, Inference.ns, Monads.ns]
    (PyHelpersSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python code generator: converts Hydra modules to Python source code"
  where
    elements = [
      toBinding useInlineTypeParamsFor,
      toBinding useInlineTypeParams,
      toBinding typeAliasStatementFor,
      toBinding unionTypeStatementsFor,
      toBinding wrapInNullaryLambda,
      toBinding wrapLazyArguments,
      toBinding pyInt,
      toBinding lruCacheDecorator,
      toBinding makeThunk,
      toBinding makeCurriedLambda,
      toBinding genericArg,
      toBinding variantArgs,
      toBinding environmentTypeParameters,
      toBinding encodeFloatValue,
      toBinding encodeIntegerValue,
      toBinding encodeLiteral,
      toBinding encodeLiteralType,
      toBinding encodeApplicationType,
      toBinding encodeForallType,
      toBinding encodeFunctionType,
      toBinding encodeType,
      toBinding encodeTypeQuoted,
      toBinding encodeNameConstants,
      toBinding findTypeParams,
      toBinding encodeWrappedType,
      toBinding extendEnvWithTypeVar,
      toBinding gatherLambdas,
      toBinding extendEnvWithLambdaParams,
      toBinding makeSimpleLambda,
      toBinding isCaseStatementApplication,
      -- Case statement encoding helpers
      toBinding isVariantUnitType,
      toBinding wildcardCaseBlock,
      toBinding enumVariantPattern,
      toBinding classVariantPatternUnit,
      toBinding classVariantPatternWithCapture,
      toBinding isCasesFull,
      toBinding variantClosedPattern,
      -- Case statement helpers
      toBinding deduplicateCaseVariables,
      toBinding eliminateUnitVar,
      toBinding encodeDefaultCaseBlock,
      toBinding encodeCaseBlock,
      -- PyGraph helpers
      toBinding pyGraphGraph,
      toBinding pyGraphMetadata,
      toBinding makePyGraph,
      toBinding inGraphContext,
      -- Type encoding
      toBinding encodeFieldType,
      toBinding dataclassDecorator,
      toBinding encodeRecordType,
      toBinding encodeEnumValueAssignment,
      toBinding encodeUnionField,
      toBinding encodeUnionType,
      toBinding encodeUnionFieldAlt,
      toBinding encodeTypeDefSingle,
      toBinding encodeTypeAssignment,
      toBinding encodeTypeAssignmentInner,
      toBinding unsupportedExpression,
      toBinding makeUncurriedLambda,
      -- Field encoding
      toBinding encodeField,
      toBinding extractCaseElimination,
      toBinding encodeBindingsAsDefs,
      toBinding encodeBindingAs,
      toBinding encodeDefinition,
      -- Coder context helpers
      toBinding updateMeta,
      toBinding withBindings,
      toBinding withUpdatedGraph,
      -- Arity helpers
      toBinding termArityWithPrimitives,
      toBinding functionArityWithPrimitives,
      -- Python function analysis
      toBinding pythonEnvironmentGetTypeContext,
      toBinding pythonEnvironmentSetTypeContext,
      -- withLambda and withTypeLambda for context management
      toBinding withLambda,
      toBinding withTypeLambda,
      -- withLet for let context management
      toBinding withLet,
      toBinding withLetInline,
      -- Metadata helpers
      toBinding initialMetadata,
      -- Initial environment creation
      toBinding initialEnvironment,
      toBinding targetPythonVersion,
      -- Function analysis
      toBinding analyzePythonFunction,
      toBinding analyzePythonFunctionInline,
      -- withDefinitions context
      toBinding withDefinitions,
      -- Binding encoding
      toBinding encodeBindingAsAssignment,
      toBinding encodeFunctionDefinition,
      toBinding encodeTermMultiline,
      toBinding encodeFunction,
      toBinding encodeTermAssignment,
      toBinding encodeVariable,
      toBinding encodeApplication,
      toBinding encodeApplicationInner,
      toBinding encodeTermInline,
      -- Metadata extension (for module encoding)
      toBinding extendMetaForTerm,
      toBinding extendMetaForType,
      toBinding digForWrap,
      toBinding setMetaNamespaces,
      toBinding setMetaUsesLeft,
      toBinding setMetaUsesRight,
      toBinding setMetaUsesDecimal,
      toBinding setMetaUsesFrozenDict,
      toBinding setMetaUsesNothing,
      toBinding setMetaUsesJust,
      toBinding setMetaUsesCallable,
      toBinding setMetaUsesLruCache,
      toBinding setMetaUsesCast,
      toBinding setMetaUsesGeneric,
      toBinding setMetaUsesFrozenList,
      toBinding setMetaUsesMaybe,
      toBinding setMetaUsesEither,
      toBinding setMetaUsesNode,
      toBinding setMetaUsesEnum,
      toBinding setMetaUsesAnnotated,
      toBinding setMetaUsesDataclass,
      toBinding setMetaTypeVariables,
      toBinding isTypeVariableName,
      toBinding collectTypeVariables,
      toBinding extendMetaForTypes,
      toBinding setMetaUsesName,
      toBinding setMetaUsesTypeVar,
      toBinding emptyMetadata,
      toBinding gatherMetadata,
      toBinding setMetaUsesTypeAlias,
      -- Module encoding
      toBinding isTypeModuleCheck,
      toBinding reorderDefs,
      toBinding tvarStatement,
      toBinding condImportSymbol,
      toBinding moduleDomainImports,
      toBinding standardImportStatement,
      toBinding moduleStandardImports,
      toBinding moduleImports,
      toBinding encodePythonModule,
      toBinding moduleToPython]

-- | Version-aware inline type parameters.
--   Python 3.12+ supports `def foo[T]()` syntax.
--   Python 3.10 requires `T = TypeVar("T"); def foo()` at module level.
useInlineTypeParamsFor :: TBinding (PyHelpers.PythonVersion -> Bool)
useInlineTypeParamsFor = def "useInlineTypeParamsFor" $
  doc "Version-aware inline type parameters" $
  "version" ~>
    Equality.equal (var "version") (inject PyHelpers._PythonVersion PyHelpers._PythonVersion_python312 unit)

-- | Legacy constant for backward compatibility
useInlineTypeParams :: TBinding Bool
useInlineTypeParams = def "useInlineTypeParams" $
  doc "Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code" $
  useInlineTypeParamsFor @@ PyUtils.targetPythonVersion

-- | Version-aware type alias statement generation.
--   Uses PEP 695 syntax for Python 3.12+, or TypeAlias syntax for Python 3.10+.
typeAliasStatementFor :: TBinding (PyHelpers.PythonEnvironment -> Py.Name -> [Py.TypeParameter] -> Maybe String -> Py.Expression -> Py.Statement)
typeAliasStatementFor = def "typeAliasStatementFor" $
  doc "Version-aware type alias statement generation" $
  "env" ~> "name" ~> "tparams" ~> "mcomment" ~> "tyexpr" ~>
    Logic.ifElse (useInlineTypeParamsFor @@ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_version @@ var "env"))
      (PyUtils.typeAliasStatement @@ var "name" @@ var "tparams" @@ var "mcomment" @@ var "tyexpr")
      (PyUtils.typeAliasStatement310 @@ var "name" @@ var "tparams" @@ var "mcomment" @@ var "tyexpr")

-- | Version-aware union type statement generation.
--   Uses PEP 695 type alias syntax for Python 3.12+.
--   For Python 3.10, generates a metaclass-based class that is subscriptable at runtime.
unionTypeStatementsFor :: TBinding (PyHelpers.PythonEnvironment -> Py.Name -> [Py.TypeParameter] -> Maybe String -> Py.Expression -> [Py.Statement])
unionTypeStatementsFor = def "unionTypeStatementsFor" $
  doc "Version-aware union type statement generation" $
  "env" ~> "name" ~> "tparams" ~> "mcomment" ~> "tyexpr" ~>
    Logic.ifElse (useInlineTypeParamsFor @@ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_version @@ var "env"))
      (list [PyUtils.typeAliasStatement @@ var "name" @@ var "tparams" @@ var "mcomment" @@ var "tyexpr"])
      (PyUtils.unionTypeClassStatements310 @@ var "name" @@ var "mcomment" @@ var "tyexpr")

-- | Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation
wrapInNullaryLambda :: TBinding (Py.Expression -> Py.Expression)
wrapInNullaryLambda = def "wrapInNullaryLambda" $
  doc "Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation" $
  "expr" ~>
    PyDsl.expressionLambda $ PyDsl.lambda_ PyDsl.lambdaParametersEmpty (var "expr")

-- | Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation
wrapLazyArguments :: TBinding (Name -> [Py.Expression] -> [Py.Expression])
wrapLazyArguments = def "wrapLazyArguments" $
  doc "Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation" $
  "name" ~> "args" ~>
    Logic.ifElse
      (Logic.and
        (Equality.equal (var "name") (Core.name $ string "hydra.lib.logic.ifElse"))
        (Equality.equal (Lists.length (var "args")) (int32 3)))
      -- For if_else, wrap arguments 2 and 3 (the then/else branches)
      (list [
        Lists.at (int32 0) (var "args"),
        wrapInNullaryLambda @@ (Lists.at (int32 1) (var "args")),
        wrapInNullaryLambda @@ (Lists.at (int32 2) (var "args"))])
      (var "args")

-- | Create integer literal expression
pyInt :: TBinding (Integer -> Py.Expression)
pyInt = def "pyInt" $
  doc "Create integer literal expression" $
  "n" ~>
    PyUtils.pyAtomToPyExpression @@
      (PyDsl.atomNumber $ PyDsl.numberInteger $ var "n")

-- | Create an expression that calls hydra.dsl.python.unsupported(message) at runtime.
-- This is used for features that cannot be directly translated to Python.
unsupportedExpression :: TBinding (String -> Py.Expression)
unsupportedExpression = def "unsupportedExpression" $
  doc "Create an expression that calls hydra.dsl.python.unsupported(message) at runtime" $
  "msg" ~>
    PyUtils.functionCall @@
      (PyUtils.pyExpressionToPyPrimary @@
        (PyUtils.projectFromExpression @@
          (PyUtils.projectFromExpression @@
            (PyUtils.projectFromExpression @@
              (PyDsl.pyNameToPyExpression $ PyDsl.name $ string "hydra") @@
              (PyDsl.name $ string "dsl")) @@
            (PyDsl.name $ string "python")) @@
          (PyDsl.name $ string "unsupported"))) @@
      list [PyUtils.stringToPyExpression @@ PyDsl.quoteStyleDouble @@ var "msg"]

-- | Decorator for @lru_cache(1) to memoize zero-argument function results
lruCacheDecorator :: TBinding Py.NamedExpression
lruCacheDecorator = def "lruCacheDecorator" $
  doc "Decorator for @lru_cache(1) to memoize zero-argument function results" $
  PyDsl.namedExpressionSimple $
    PyUtils.functionCall @@
      (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "lru_cache") @@
      list [pyInt @@ bigint 1]

-- | Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization
makeThunk :: TBinding (Py.Expression -> Py.Expression)
makeThunk = def "makeThunk" $
  doc "Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization" $
  "pbody" ~>
    PyUtils.functionCall @@
      (PyUtils.pyExpressionToPyPrimary @@
        (PyUtils.functionCall @@
          (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "lru_cache") @@
          list [pyInt @@ bigint 1])) @@
      list [wrapInNullaryLambda @@ var "pbody"]

-- | Create an uncurried lambda with multiple parameters
-- | e.g., makeUncurriedLambda [p1, p2, p3] body => lambda p1, p2, p3: body
makeUncurriedLambda :: TBinding ([Py.Name] -> Py.Expression -> Py.Expression)
makeUncurriedLambda = def "makeUncurriedLambda" $
  doc "Create an uncurried lambda with multiple parameters" $
  "params" ~> "body" ~>
    PyDsl.expressionLambda $ PyDsl.lambda_
      (PyDsl.lambdaParametersSimple $ Lists.map ("p" ~> PyDsl.lambdaParamNoDefault (var "p")) (var "params"))
      (var "body")

-- | Create a curried lambda chain from a list of parameter names and a body
makeCurriedLambda :: TBinding ([Py.Name] -> Py.Expression -> Py.Expression)
makeCurriedLambda = def "makeCurriedLambda" $
  doc "Create a curried lambda chain from a list of parameter names and a body" $
  "params" ~> "body" ~>
    Lists.foldl
      ("acc" ~> "p" ~>
        PyDsl.expressionLambda $ PyDsl.lambda_
          (PyDsl.lambdaParametersSimple $ list [PyDsl.lambdaParamNoDefault $ var "p"])
          (var "acc"))
      (var "body")
      (Lists.reverse (var "params"))

-- | Create Generic[...] argument expression for class definition
genericArg :: TBinding ([Name] -> Maybe Py.Expression)
genericArg = def "genericArg" $
  doc "Create Generic[...] argument expression for class definition" $
  "tparamList" ~>
    Logic.ifElse (Lists.null (var "tparamList"))
      nothing
      (just $
        PyUtils.pyPrimaryToPyExpression @@
          (PyUtils.primaryWithExpressionSlices @@
            (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Generic") @@
            (Lists.map ("n" ~> PyDsl.pyNameToPyExpression (PyNames.encodeTypeVariable @@ var "n")) (var "tparamList"))))

-- | Create args for variant (Node[type], Generic[tparams])
variantArgs :: TBinding (Py.Expression -> [Name] -> Py.Args)
variantArgs = def "variantArgs" $
  doc "Create args for variant (Node[type], Generic[tparams])" $
  "ptype" ~> "tparams" ~>
    PyUtils.pyExpressionsToPyArgs @@
      (Maybes.cat (list [
        just $ PyUtils.pyPrimaryToPyExpression @@
          (PyUtils.primaryWithExpressionSlices @@
            (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Node") @@
            list [var "ptype"]),
        genericArg @@ var "tparams"]))

-- | Get type parameters from environment as Python TypeParameters
environmentTypeParameters :: TBinding (PyHelpers.PythonEnvironment -> [Py.TypeParameter])
environmentTypeParameters = def "environmentTypeParameters" $
  doc "Get type parameters from environment as Python TypeParameters" $
  "env" ~>
    Lists.map
      (PyUtils.pyNameToPyTypeParameter <.> PyNames.encodeTypeVariable)
      (Pairs.first (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env"))

-- | Encode a float value to a Python expression
encodeFloatValue :: TBinding (FloatValue -> Flow s Py.Expression)
encodeFloatValue = def "encodeFloatValue" $
  doc "Encode a float value to a Python expression" $
  "fv" ~>
    cases _FloatValue (var "fv") Nothing [
      _FloatValue_bigfloat>>: "f" ~>
        produce $ PyUtils.functionCall @@
          (PyUtils.pyNameToPyPrimary @@ (PyDsl.name $ string "Decimal")) @@
          list [PyUtils.singleQuotedString @@ (Literals.showBigfloat $ var "f")],
      _FloatValue_float32>>: "f" ~>
        produce $ PyUtils.pyAtomToPyExpression @@
          (PyDsl.atomNumber $ PyDsl.numberFloat $ Literals.float32ToBigfloat $ var "f"),
      _FloatValue_float64>>: "f" ~>
        produce $ PyUtils.pyAtomToPyExpression @@
          (PyDsl.atomNumber $ PyDsl.numberFloat $ Literals.float64ToBigfloat $ var "f")]

-- | Encode an integer value to a Python expression
encodeIntegerValue :: TBinding (IntegerValue -> Flow s Py.Expression)
encodeIntegerValue = def "encodeIntegerValue" $
  doc "Encode an integer value to a Python expression" $
  "iv" ~>
    -- All integer types map to Python int (arbitrary precision)
    "toPyInt" <~ ("n" ~>
      produce $ PyUtils.pyAtomToPyExpression @@
        (PyDsl.atomNumber $ PyDsl.numberInteger $ var "n")) $
    cases _IntegerValue (var "iv") Nothing [
      _IntegerValue_bigint>>: "i" ~> var "toPyInt" @@ var "i",
      _IntegerValue_int8>>: "i" ~> var "toPyInt" @@ (Literals.int8ToBigint $ var "i"),
      _IntegerValue_int16>>: "i" ~> var "toPyInt" @@ (Literals.int16ToBigint $ var "i"),
      _IntegerValue_int32>>: "i" ~> var "toPyInt" @@ (Literals.int32ToBigint $ var "i"),
      _IntegerValue_int64>>: "i" ~> var "toPyInt" @@ (Literals.int64ToBigint $ var "i"),
      _IntegerValue_uint8>>: "i" ~> var "toPyInt" @@ (Literals.uint8ToBigint $ var "i"),
      _IntegerValue_uint16>>: "i" ~> var "toPyInt" @@ (Literals.uint16ToBigint $ var "i"),
      _IntegerValue_uint32>>: "i" ~> var "toPyInt" @@ (Literals.uint32ToBigint $ var "i"),
      _IntegerValue_uint64>>: "i" ~> var "toPyInt" @@ (Literals.uint64ToBigint $ var "i")]

-- | Encode a literal value to a Python expression
encodeLiteral :: TBinding (Literal -> Flow s Py.Expression)
encodeLiteral = def "encodeLiteral" $
  doc "Encode a literal value to a Python expression" $
  "lit" ~>
    cases _Literal (var "lit") Nothing [
      _Literal_binary>>: "bs" ~>
        -- Convert binary to list of byte values (0-255) directly, without base64 encoding
        "byteValues" <~ Literals.binaryToBytes (var "bs") $
        produce $ PyUtils.functionCall @@
          (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "bytes") @@
          list [PyUtils.pyAtomToPyExpression @@
            (PyDsl.atomList $
              PyUtils.pyList @@ (Lists.map
                ("byteVal" ~>
                  PyUtils.pyAtomToPyExpression @@
                    (PyDsl.atomNumber $ PyDsl.numberInteger $ Literals.int32ToBigint $ var "byteVal"))
                (var "byteValues")))],
      _Literal_boolean>>: "b" ~>
        produce $ PyUtils.pyAtomToPyExpression @@
          Logic.ifElse (var "b") PyDsl.atomTrue PyDsl.atomFalse,
      _Literal_float>>: "f" ~> encodeFloatValue @@ var "f",
      _Literal_integer>>: "i" ~> encodeIntegerValue @@ var "i",
      _Literal_string>>: "s" ~>
        produce $ PyUtils.stringToPyExpression @@ PyDsl.quoteStyleDouble @@ var "s"]

-- | Encode a literal type to a Python type expression
encodeLiteralType :: TBinding (LiteralType -> Flow s Py.Expression)
encodeLiteralType = def "encodeLiteralType" $
  doc "Encode a literal type to a Python type expression" $
  "lt" ~>
    "findName" <~ (cases _LiteralType (var "lt") Nothing [
      _LiteralType_binary>>: constant $ string "bytes",
      _LiteralType_boolean>>: constant $ string "bool",
      _LiteralType_float>>: "ft" ~>
        cases _FloatType (var "ft") Nothing [
          _FloatType_bigfloat>>: constant $ string "Decimal",
          _FloatType_float32>>: constant $ string "float",
          _FloatType_float64>>: constant $ string "float"],
      _LiteralType_integer>>: constant $ string "int",
      _LiteralType_string>>: constant $ string "str"]) $
    produce $ PyDsl.pyNameToPyExpression $ PyDsl.name $ var "findName"

-- | Encode an application type to Python expression.
--   Gathers all type arguments and encodes as primary[args].
encodeApplicationType :: TBinding (PyHelpers.PythonEnvironment -> ApplicationType -> Flow PyHelpers.PyGraph Py.Expression)
encodeApplicationType = def "encodeApplicationType" $
  doc "Encode an application type to Python expression" $
  "env" ~> "at" ~>
    -- gatherParams collects all arguments from nested applications
    "gatherParams" <~ (
      "t" ~> "ps" ~>
        cases _Type (Rewriting.deannotateType @@ var "t") Nothing [
          _Type_application>>: "appT" ~>
            var "gatherParams"
              @@ (project _ApplicationType _ApplicationType_function @@ var "appT")
              @@ (Lists.cons (project _ApplicationType _ApplicationType_argument @@ var "appT") (var "ps")),
          -- Default: return (t, ps)
          _Type_annotated>>: constant $ pair (var "t") (var "ps"),
          _Type_function>>: constant $ pair (var "t") (var "ps"),
          _Type_forall>>: constant $ pair (var "t") (var "ps"),
          _Type_list>>: constant $ pair (var "t") (var "ps"),
          _Type_literal>>: constant $ pair (var "t") (var "ps"),
          _Type_map>>: constant $ pair (var "t") (var "ps"),
          _Type_maybe>>: constant $ pair (var "t") (var "ps"),
          _Type_either>>: constant $ pair (var "t") (var "ps"),
          _Type_pair>>: constant $ pair (var "t") (var "ps"),
          _Type_record>>: constant $ pair (var "t") (var "ps"),
          _Type_set>>: constant $ pair (var "t") (var "ps"),
          _Type_union>>: constant $ pair (var "t") (var "ps"),
          _Type_unit>>: constant $ pair (var "t") (var "ps"),
          _Type_variable>>: constant $ pair (var "t") (var "ps"),
          _Type_wrap>>: constant $ pair (var "t") (var "ps")]) $
    "bodyAndArgs" <~ (var "gatherParams" @@ (inject _Type _Type_application $ var "at") @@ list ([] :: [TTerm Type])) $
    "body" <~ Pairs.first (var "bodyAndArgs") $
    "args" <~ Pairs.second (var "bodyAndArgs") $
    "pyBody" <<~ encodeType @@ var "env" @@ var "body" $
    "pyArgs" <<~ Flows.mapList (encodeType @@ var "env") (var "args") $
    produce $ PyUtils.primaryAndParams @@ (PyUtils.pyExpressionToPyPrimary @@ var "pyBody") @@ var "pyArgs"

-- | Encode a forall type to Python expression.
--   Gathers all type parameters and encodes the body with parameters.
encodeForallType :: TBinding (PyHelpers.PythonEnvironment -> ForallType -> Flow PyHelpers.PyGraph Py.Expression)
encodeForallType = def "encodeForallType" $
  doc "Encode a forall type to Python expression" $
  "env" ~> "lt" ~>
    -- gatherParams collects all forall-bound type variables
    "gatherParams" <~ (
      "t" ~> "ps" ~>
        cases _Type (Rewriting.deannotateType @@ var "t") Nothing [
          _Type_forall>>: "forallT" ~>
            var "gatherParams"
              @@ (project _ForallType _ForallType_body @@ var "forallT")
              @@ (Lists.cons (project _ForallType _ForallType_parameter @@ var "forallT") (var "ps")),
          -- Default: return (t, reverse ps)
          _Type_annotated>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_application>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_function>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_list>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_literal>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_map>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_maybe>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_either>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_pair>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_record>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_set>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_union>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_unit>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_variable>>: constant $ pair (var "t") (Lists.reverse (var "ps")),
          _Type_wrap>>: constant $ pair (var "t") (Lists.reverse (var "ps"))]) $
    "bodyAndParams" <~ (var "gatherParams" @@ (inject _Type _Type_forall $ var "lt") @@ list ([] :: [TTerm Name])) $
    "body" <~ Pairs.first (var "bodyAndParams") $
    "params" <~ Pairs.second (var "bodyAndParams") $
    "pyBody" <<~ encodeType @@ var "env" @@ var "body" $
    produce $ PyUtils.primaryAndParams
      @@ (PyUtils.pyExpressionToPyPrimary @@ var "pyBody")
      @@ (Lists.map ("n" ~> PyDsl.pyNameToPyExpression $ PyDsl.name $ Core.unName (var "n")) (var "params"))

-- | Encode a function type to Python Callable[..., return_type].
--   Gathers all domain types and the final codomain.
encodeFunctionType :: TBinding (PyHelpers.PythonEnvironment -> FunctionType -> Flow PyHelpers.PyGraph Py.Expression)
encodeFunctionType = def "encodeFunctionType" $
  doc "Encode a function type to Python Callable expression" $
  "env" ~> "ft" ~>
    -- gatherParams collects all domain types and final codomain
    "gatherParams" <~ (
      "rdoms" ~> "ftype" ~>
        "innerCod" <~ (project _FunctionType _FunctionType_codomain @@ var "ftype") $
        "dom" <~ (project _FunctionType _FunctionType_domain @@ var "ftype") $
        cases _Type (Rewriting.deannotateType @@ var "innerCod") Nothing [
          _Type_function>>: "ft2" ~>
            var "gatherParams" @@ (Lists.cons (var "dom") (var "rdoms")) @@ var "ft2",
          -- Default: return (reverse (dom:rdoms), innerCod)
          _Type_annotated>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_application>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_forall>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_list>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_literal>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_map>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_maybe>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_either>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_pair>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_record>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_set>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_union>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_unit>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_variable>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod"),
          _Type_wrap>>: constant $ pair (Lists.reverse (Lists.cons (var "dom") (var "rdoms"))) (var "innerCod")]) $
    "domsAndCod" <~ (var "gatherParams" @@ list ([] :: [TTerm Type]) @@ var "ft") $
    "doms" <~ Pairs.first (var "domsAndCod") $
    "cod" <~ Pairs.second (var "domsAndCod") $
    "pydoms" <<~ Flows.mapList (encodeType @@ var "env") (var "doms") $
    "pycod" <<~ encodeType @@ var "env" @@ var "cod" $
    produce $ PyUtils.pyPrimaryToPyExpression @@
      (PyUtils.primaryWithSlices
        @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Callable")
        @@ (PyUtils.pyPrimaryToPySlice @@
            (PyDsl.primarySimple $ PyDsl.atomList $ PyUtils.pyList @@ var "pydoms"))
        @@ list [PyDsl.sliceOrStarredExpressionSlice $ PyUtils.pyExpressionToPySlice @@ var "pycod"])

-- | Encode a Hydra type to a Python type expression.
--   This is the main recursive type encoder.
encodeType :: TBinding (PyHelpers.PythonEnvironment -> Type -> Flow PyHelpers.PyGraph Py.Expression)
encodeType = def "encodeType" $
  doc "Encode a Hydra type to a Python type expression" $
  "env" ~> "typ" ~>
    -- dflt produces a quoted string fallback for unsupported types
    "dflt" <~ (produce $ PyUtils.doubleQuotedString @@ (Strings.cat2 (string "type = ") (ShowCore.type_ @@ (Rewriting.deannotateType @@ var "typ")))) $
    cases _Type (Rewriting.deannotateType @@ var "typ") Nothing [
      _Type_application>>: "at" ~> encodeApplicationType @@ var "env" @@ var "at",
      _Type_function>>: "ft" ~> encodeFunctionType @@ var "env" @@ var "ft",
      _Type_forall>>: "lt" ~> encodeForallType @@ var "env" @@ var "lt",
      _Type_list>>: "et" ~>
        "pyet" <<~ encodeType @@ var "env" @@ var "et" $
        produce $ PyUtils.nameAndParams @@ (PyDsl.name $ string "frozenlist") @@ list [var "pyet"],
      _Type_map>>: "mt" ~>
        "pykt" <<~ encodeType @@ var "env" @@ (project _MapType _MapType_keys @@ var "mt") $
        "pyvt" <<~ encodeType @@ var "env" @@ (project _MapType _MapType_values @@ var "mt") $
        produce $ PyUtils.nameAndParams @@ (PyDsl.name $ string "FrozenDict") @@ list [var "pykt", var "pyvt"],
      _Type_literal>>: "lt" ~> encodeLiteralType @@ var "lt",
      _Type_maybe>>: "et" ~>
        "ptype" <<~ encodeType @@ var "env" @@ var "et" $
        produce $ PyUtils.pyPrimaryToPyExpression @@
          (PyUtils.primaryWithExpressionSlices @@
            (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Maybe") @@
            list [var "ptype"]),
      _Type_either>>: "eitherT" ~>
        "pyleft" <<~ encodeType @@ var "env" @@ (project _EitherType _EitherType_left @@ var "eitherT") $
        "pyright" <<~ encodeType @@ var "env" @@ (project _EitherType _EitherType_right @@ var "eitherT") $
        produce $ PyUtils.pyPrimaryToPyExpression @@
          (PyUtils.primaryWithExpressionSlices @@
            (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "Either") @@
            list [var "pyleft", var "pyright"]),
      _Type_pair>>: "pairT" ~>
        "pyFirst" <<~ encodeType @@ var "env" @@ (project _PairType _PairType_first @@ var "pairT") $
        "pySecond" <<~ encodeType @@ var "env" @@ (project _PairType _PairType_second @@ var "pairT") $
        produce $ PyUtils.nameAndParams @@ (PyDsl.name $ string "tuple") @@ list [var "pyFirst", var "pySecond"],
      _Type_record>>: "rt" ~>
        produce $ PyNames.typeVariableReference @@ var "env" @@ (project _RowType _RowType_typeName @@ var "rt"),
      _Type_set>>: "et" ~>
        "pyet" <<~ encodeType @@ var "env" @@ var "et" $
        produce $ PyUtils.nameAndParams @@ (PyDsl.name $ string "frozenset") @@ list [var "pyet"],
      _Type_union>>: "rt" ~>
        produce $ PyNames.typeVariableReference @@ var "env" @@ (project _RowType _RowType_typeName @@ var "rt"),
      _Type_unit>>: constant $ produce $ PyUtils.pyNameToPyExpression @@ PyUtils.pyNone,
      _Type_variable>>: "name" ~>
        produce $ PyNames.typeVariableReference @@ var "env" @@ var "name",
      _Type_wrap>>: "wt" ~>
        produce $ PyNames.typeVariableReference @@ var "env" @@ (project _WrappedType _WrappedType_typeName @@ var "wt"),
      -- Default case for annotated and any other types
      _Type_annotated>>: constant $ var "dflt"]

-- | Encode a type to a Python expression, quoting if the type has free variables.
--   Free variables indicate forward references that need to be quoted strings in Python.
encodeTypeQuoted :: TBinding (PyHelpers.PythonEnvironment -> Type -> Flow PyHelpers.PyGraph Py.Expression)
encodeTypeQuoted = def "encodeTypeQuoted" $
  doc "Encode a type to a Python expression, quoting if the type has free variables" $
  "env" ~> "typ" ~>
    "pytype" <<~ encodeType @@ var "env" @@ var "typ" $
    produce $ Logic.ifElse (Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
      (var "pytype")
      (PyUtils.doubleQuotedString @@ (Serialization.printExpr @@ (PySerde.encodeExpression @@ var "pytype")))

-- | Generate name constants for a type: _TypeName and _TypeName_fieldName for each field.
--   These constants allow field access via string constants for serialization/deserialization.
encodeNameConstants :: TBinding (PyHelpers.PythonEnvironment -> Name -> Type -> [Py.Statement])
encodeNameConstants = def "encodeNameConstants" $
  doc "Generate name constants for a type" $
  "env" ~> "name" ~> "typ" ~>
    -- Helper to create a statement from a (pyName, hname) pair
    "toStmt" <~ ("pair" ~>
      PyUtils.assignmentStatement
        @@ (Pairs.first $ var "pair")
        @@ (PyUtils.functionCall
              @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeName @@ true @@ Util.caseConventionPascal @@ var "env" @@ (Core.name $ string "hydra.core.Name")))
              @@ list [PyUtils.doubleQuotedString @@ (Core.unName $ Pairs.second $ var "pair")])) $
    -- The name constant for the type itself
    "namePair" <~ pair (PyNames.encodeConstantForTypeName @@ var "env" @@ var "name") (var "name") $
    -- Create field constant pairs from field types
    "fieldPair" <~ ("field" ~>
      pair
        (PyNames.encodeConstantForFieldName @@ var "env" @@ var "name" @@ (project _FieldType _FieldType_name @@ var "field"))
        (project _FieldType _FieldType_name @@ var "field")) $
    -- Extract field pairs from the type by traversing foralls to get to record/union
    "fieldPairs" <~ ("t" ~>
      cases _Type (Rewriting.deannotateType @@ var "t") Nothing [
        _Type_forall>>: "ft" ~> var "fieldPairs" @@ (project _ForallType _ForallType_body @@ var "ft"),
        _Type_record>>: "rt" ~> Lists.map (var "fieldPair") (project _RowType _RowType_fields @@ var "rt"),
        _Type_union>>: "rt" ~> Lists.map (var "fieldPair") (project _RowType _RowType_fields @@ var "rt"),
        -- All other types have no fields
        _Type_annotated>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_application>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_function>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_list>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_literal>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_map>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_maybe>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_either>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_pair>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_set>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_unit>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_variable>>: constant $ list ([] :: [TTerm (Py.Name, Name)]),
        _Type_wrap>>: constant $ list ([] :: [TTerm (Py.Name, Name)])]) $
    Lists.map (var "toStmt") (Lists.cons (var "namePair") (var "fieldPairs" @@ var "typ"))

-- | Find type parameters in a type that are bound in the environment.
--   Returns the free type variables that are also in the bound type variables map.
findTypeParams :: TBinding (PyHelpers.PythonEnvironment -> Type -> [Name])
findTypeParams = def "findTypeParams" $
  doc "Find type parameters in a type that are bound in the environment" $
  "env" ~> "typ" ~>
    "boundVars" <~ (Pairs.second $ project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env") $
    "isBound" <~ ("v" ~> Maybes.isJust (Maps.lookup (var "v") (var "boundVars"))) $
    Lists.filter (var "isBound") (Sets.toList (Rewriting.freeVariablesInType @@ var "typ"))

-- | Encode a wrapped type (newtype) to a Python class definition.
--   Creates a class that extends Node[inner_type] with optional Generic[T] for polymorphic types.
encodeWrappedType :: TBinding (PyHelpers.PythonEnvironment -> Name -> Type -> Maybe String -> Flow PyHelpers.PyGraph Py.Statement)
encodeWrappedType = def "encodeWrappedType" $
  doc "Encode a wrapped type (newtype) to a Python class definition" $
  "env" ~> "name" ~> "typ" ~> "comment" ~>
    "tparamList" <~ (Pairs.first $ project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env") $
    "ptypeQuoted" <<~ encodeTypeQuoted @@ var "env" @@ var "typ" $
    "body" <~ (PyUtils.indentedBlock @@ var "comment" @@ list ([] :: [TTerm [Py.Statement]])) $
    produce $ PyUtils.pyClassDefinitionToPyStatement @@
      PyDsl.classDefinition
        nothing
        (PyNames.encodeName @@ false @@ Util.caseConventionPascal @@ var "env" @@ var "name")
        (Lists.map (PyUtils.pyNameToPyTypeParameter <.> PyNames.encodeTypeVariable) (findTypeParams @@ var "env" @@ var "typ"))
        (just (variantArgs @@ var "ptypeQuoted" @@ var "tparamList"))
        (var "body")

-- | Extend a PythonEnvironment with a new bound type variable.
--   This creates a new environment with the variable added to the type parameter list and map.
extendEnvWithTypeVar :: TBinding (PyHelpers.PythonEnvironment -> Name -> PyHelpers.PythonEnvironment)
extendEnvWithTypeVar = def "extendEnvWithTypeVar" $
  doc "Extend a PythonEnvironment with a new bound type variable" $
  "env" ~> "var_" ~>
    "oldBound" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env") $
    "tparamList" <~ (Pairs.first $ var "oldBound") $
    "tparamMap" <~ (Pairs.second $ var "oldBound") $
    "newList" <~ (Lists.concat2 (var "tparamList") (list [var "var_"])) $
    "newMap" <~ (Maps.insert (var "var_") (PyNames.encodeTypeVariable @@ var "var_") (var "tparamMap")) $
    record PyHelpers._PythonEnvironment [
      PyHelpers._PythonEnvironment_namespaces>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "env",
      PyHelpers._PythonEnvironment_boundTypeVariables>>: pair (var "newList") (var "newMap"),
      PyHelpers._PythonEnvironment_typeContext>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env",
      PyHelpers._PythonEnvironment_nullaryBindings>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_nullaryBindings @@ var "env",
      PyHelpers._PythonEnvironment_version>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_version @@ var "env",
      PyHelpers._PythonEnvironment_skipCasts>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_skipCasts @@ var "env",
      PyHelpers._PythonEnvironment_inlineVariables>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_inlineVariables @@ var "env"]

-- | Extract lambdas and their bodies from a term.
--   Returns the list of lambda parameters (in order from outermost to innermost) and the innermost body.
gatherLambdas :: TBinding (Term -> ([Name], Term))
gatherLambdas = def "gatherLambdas" $
  doc "Extract lambdas and their bodies from a term" $
  "term" ~>
  "go" <~ ("params" ~> "t" ~>
    cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "t")
      (Just $ pair (var "params") (var "t")) [
      _Term_function>>: "f" ~>
        cases _Function (var "f")
          (Just $ pair (var "params") (var "t")) [
          _Function_lambda>>: "l" ~>
            var "go" @@ Lists.concat2 (var "params") (list [Core.lambdaParameter $ var "l"])
                     @@ Core.lambdaBody (var "l")]]) $
  var "go" @@ list ([] :: [TTerm Name]) @@ var "term"

-- | Wrap a bare reference to a polymorphic function in an uncurried lambda,
--   avoiding pyright errors due to confusion about type parameters.
--   Creates: lambda x1, x2, ...: f(x1, x2, ...)
makeSimpleLambda :: TBinding (Int -> Py.Expression -> Py.Expression)
makeSimpleLambda = def "makeSimpleLambda" $
  doc "Wrap a bare reference to a polymorphic function in an uncurried lambda" $
  "arity" ~> "lhs" ~>
    "args" <~ (Lists.map
      ("i" ~> PyDsl.name (string "x" ++ Literals.showInt32 (var "i")))
      (Math.range (int32 1) (var "arity"))) $
    Logic.ifElse (Equality.equal (var "arity") (int32 0))
      (var "lhs")
      (PyDsl.expressionLambda $
        PyDsl.lambda_
          (PyDsl.lambdaParametersSimple $ Lists.map ("a" ~> PyDsl.lambdaParamNoDefault (var "a")) (var "args"))
          (PyUtils.functionCall @@
            (PyUtils.pyExpressionToPyPrimary @@ var "lhs") @@
            (Lists.map ("a" ~> PyDsl.pyNameToPyExpression (var "a")) (var "args"))))

-- | Check if a term is a case statement applied to exactly one argument.
--   Returns Just (tname, dflt, cases, arg) if so, Nothing otherwise.
isCaseStatementApplication :: TBinding (Term -> Maybe (Name, Maybe Term, [Field], Term))
isCaseStatementApplication = def "isCaseStatementApplication" $
  doc "Check if a term is a case statement applied to exactly one argument" $
  "term" ~>
    "gathered" <~ (CoderUtils.gatherApplications @@ var "term") $
    "args" <~ (Pairs.first $ var "gathered") $
    "body" <~ (Pairs.second $ var "gathered") $
    -- Check for exactly one argument
    Logic.ifElse (Logic.not $ Equality.equal (Lists.length $ var "args") (int32 1))
      nothing
      ("arg" <~ (Lists.head $ var "args") $
        cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "body") (Just nothing) [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just nothing) [
              _Function_elimination>>: "e" ~>
                cases _Elimination (var "e") (Just nothing) [
                  _Elimination_union>>: "cs" ~>
                    just $ Phantoms.tuple4
                      (Core.caseStatementTypeName $ var "cs")
                      (Core.caseStatementDefault $ var "cs")
                      (Core.caseStatementCases $ var "cs")
                      (var "arg")]]])

-- | Extend PythonEnvironment's TypeContext by adding lambda parameters.
--   This is used when we've gathered lambdas from a term but need to manually extend the context.
extendEnvWithLambdaParams :: TBinding (PyHelpers.PythonEnvironment -> Term -> PyHelpers.PythonEnvironment)
extendEnvWithLambdaParams = def "extendEnvWithLambdaParams" $
  doc "Extend environment with lambda parameters from a term" $
  "env" ~> "term" ~>
    "go" <~ ("e" ~> "t" ~>
      cases _Term (Rewriting.deannotateTerm @@ var "t") (Just $ var "e") [
        _Term_function>>: "f" ~>
          cases _Function (var "f") (Just $ var "e") [
            _Function_lambda>>: "lam" ~>
              "newTc" <~ (Schemas.extendTypeContextForLambda @@
                (pythonEnvironmentGetTypeContext @@ var "e") @@ var "lam") $
              "newEnv" <~ (pythonEnvironmentSetTypeContext @@ var "newTc" @@ var "e") $
              var "go" @@ var "newEnv" @@ (Core.lambdaBody $ var "lam")]]) $
    var "go" @@ var "env" @@ var "term"

-- =============================================================================
-- Helper functions for case statement encoding (used by encodeBindingAs and encodeTermMultiline)
-- =============================================================================

-- | Extract CaseStatement from a term if it's a case elimination function.
--   Returns Just the CaseStatement if the term is a case elimination, Nothing otherwise.
extractCaseElimination :: TBinding (Term -> Maybe CaseStatement)
extractCaseElimination = def "extractCaseElimination" $
  doc "Extract CaseStatement from a case elimination term" $
  "term" ~>
    cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "term") (Just nothing) [
      _Term_function>>: "f" ~>
        cases _Function (var "f") (Just nothing) [
          _Function_elimination>>: "e" ~>
            cases _Elimination (var "e") (Just nothing) [
              _Elimination_union>>: "cs" ~> just (var "cs")]]]

-- | Check if a field type in a row type represents a unit-valued variant.
--   Used to determine if a variant has "no value" (unit type).
isVariantUnitType :: TBinding (RowType -> Name -> Bool)
isVariantUnitType = def "isVariantUnitType" $
  doc "Check if a variant field has unit type" $
  "rowType" ~> "fieldName" ~>
    "fields" <~ (project _RowType _RowType_fields @@ var "rowType") $
    "mfield" <~ (Lists.find ("ft" ~> Equality.equal (Core.fieldTypeName $ var "ft") (var "fieldName")) (var "fields")) $
    Maybes.fromMaybe false $
      Maybes.map
        ("ft" ~> Schemas.isUnitType @@ (Rewriting.deannotateType @@ Core.fieldTypeType (var "ft")))
        (var "mfield")

-- | Create a wildcard case block with a given body statement.
--   This is used for default cases in match statements.
wildcardCaseBlock :: TBinding (Py.Statement -> Py.CaseBlock)
wildcardCaseBlock = def "wildcardCaseBlock" $
  doc "Create a wildcard case block with a given body statement" $
  "stmt" ~>
    PyDsl.caseBlock
      (PyUtils.pyClosedPatternToPyPatterns @@ PyDsl.closedPatternWildcard)
      nothing
      (PyUtils.indentedBlock @@ nothing @@ list [list [var "stmt"]])

-- | Create a CaseBlock pattern for an enum variant (value pattern).
enumVariantPattern :: TBinding (PyHelpers.PythonEnvironment -> Name -> Name -> Py.ClosedPattern)
enumVariantPattern = def "enumVariantPattern" $
  doc "Create a value pattern for an enum variant" $
  "env" ~> "typeName" ~> "fieldName" ~>
    PyDsl.closedPatternValue $ PyDsl.valuePattern $ PyDsl.attribute $ list [
      PyNames.encodeName @@ true @@ Util.caseConventionPascal @@ var "env" @@ var "typeName",
      PyNames.encodeEnumValue @@ var "env" @@ var "fieldName"]

-- | Create a CaseBlock pattern for a class variant with no capture (unit variant).
classVariantPatternUnit :: TBinding (PyHelpers.PythonEnvironment -> Name -> Name -> Py.ClosedPattern)
classVariantPatternUnit = def "classVariantPatternUnit" $
  doc "Create a class pattern for a unit variant (no value captured)" $
  "env" ~> "typeName" ~> "fieldName" ~>
    PyDsl.closedPatternClass $
      PyDsl.classPatternSimple
        (PyDsl.nameOrAttribute $ list [PyNames.variantName @@ true @@ var "env" @@ var "typeName" @@ var "fieldName"])

-- | Create a CaseBlock pattern for a class variant with value capture.
classVariantPatternWithCapture :: TBinding (PyHelpers.PythonEnvironment -> Name -> Name -> Name -> Py.ClosedPattern)
classVariantPatternWithCapture = def "classVariantPatternWithCapture" $
  doc "Create a class pattern for a variant with captured value" $
  "env" ~> "typeName" ~> "fieldName" ~> "varName" ~>
    "pyVarName" <~ (PyDsl.nameOrAttribute $ list [PyNames.variantName @@ true @@ var "env" @@ var "typeName" @@ var "fieldName"]) $
    "capturePattern" <~ (PyDsl.closedPatternCapture $ PyDsl.capturePattern $
      PyDsl.patternCaptureTarget (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "varName")) $
    "keywordPattern" <~ (PyDsl.keywordPattern (PyDsl.name $ string "value") $
      PyDsl.patternOr $ PyDsl.orPattern $ list [var "capturePattern"]) $
    PyDsl.closedPatternClass $
      PyDsl.classPatternWithKeywords
        (var "pyVarName")
        (PyDsl.keywordPatterns $ list [var "keywordPattern"])

-- | Determine whether a union type's cases are fully covered.
--   Returns true if the number of cases >= number of fields in the row type.
isCasesFull :: TBinding (RowType -> [Field] -> Bool)
isCasesFull = def "isCasesFull" $
  doc "Check if union cases are fully covered" $
  "rowType" ~> "cases_" ~>
    "numCases" <~ (Lists.length $ var "cases_") $
    "numFields" <~ (Lists.length $ project _RowType _RowType_fields @@ var "rowType") $
    -- numCases >= numFields is equivalent to NOT (numCases < numFields)
    Logic.not $ Equality.lt (var "numCases") (var "numFields")

-- | Create a ClosedPattern for a variant, choosing the appropriate pattern type
--   based on whether the variant is an enum, unit type, or has a value.
variantClosedPattern :: TBinding (PyHelpers.PythonEnvironment -> Name -> Name -> RowType -> Bool -> Name -> Bool -> Py.ClosedPattern)
variantClosedPattern = def "variantClosedPattern" $
  doc "Create a ClosedPattern for a variant based on its characteristics" $
  "env" ~> "typeName" ~> "fieldName" ~> "rowType" ~> "isEnum" ~> "varName" ~> "shouldCapture" ~>
    Logic.ifElse (var "isEnum")
      (enumVariantPattern @@ var "env" @@ var "typeName" @@ var "fieldName")
      (Logic.ifElse (Logic.not $ var "shouldCapture")
        (classVariantPatternUnit @@ var "env" @@ var "typeName" @@ var "fieldName")
        (classVariantPatternWithCapture @@ var "env" @@ var "typeName" @@ var "fieldName" @@ var "varName"))

-- =============================================================================
-- Case statement helpers
-- =============================================================================

-- | Rewrite case statements in which the top-level lambda variables are re-used.
--   Such case statements are legal in Hydra, but may lead to variable name collision in Python.
--   For example: cases _Type Nothing [_Type_list>>: "t" ~> ..., _Type_set>>: "t" ~> ...]
--   In Python, both branches would bind "t", so we rename them to "t1", "t2", etc.
deduplicateCaseVariables :: TBinding ([Field] -> [Field])
deduplicateCaseVariables = def "deduplicateCaseVariables" $
  doc "Rewrite case statements to avoid variable name collisions" $
  "cases_" ~>
    -- rewriteCase: (countByName, done) -> field -> (updatedCount, updatedDone)
    "rewriteCase" <~ (
      "state" ~> "field" ~>
        "countByName" <~ Pairs.first (var "state") $
        "done" <~ Pairs.second (var "state") $
        "fname" <~ Core.fieldName (var "field") $
        "fterm" <~ Core.fieldTerm (var "field") $
        -- Check if term is a lambda
        cases _Term (Rewriting.deannotateTerm @@ var "fterm") (Just $ pair (var "countByName") (Lists.cons (var "field") (var "done"))) [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just $ pair (var "countByName") (Lists.cons (var "field") (var "done"))) [
              _Function_lambda>>: "lam" ~>
                "v" <~ Core.lambdaParameter (var "lam") $
                "mdom" <~ Core.lambdaDomain (var "lam") $
                "body" <~ Core.lambdaBody (var "lam") $
                -- Check if variable name already seen - use optCases for pattern matching
                optCases (Maps.lookup (var "v") (var "countByName"))
                  -- First occurrence: insert with count 1
                  (pair (Maps.insert (var "v") (int32 1) (var "countByName"))
                        (Lists.cons (var "field") (var "done")))
                  -- Already seen: rename variable
                  ("count" ~>
                    "count2" <~ (Math.add (var "count") (int32 1)) $
                    "v2" <~ (Core.name $ Strings.cat2 (Core.unName $ var "v") (Literals.showInt32 $ var "count2")) $
                    "newBody" <~ (Reduction.alphaConvert @@ var "v" @@ var "v2" @@ var "body") $
                    "newLam" <~ (Core.lambda (var "v2") (var "mdom") (var "newBody")) $
                    "newTerm" <~ (inject _Term _Term_function $ inject _Function _Function_lambda $ var "newLam") $
                    "newField" <~ (Core.field (var "fname") (var "newTerm")) $
                    pair (Maps.insert (var "v") (var "count2") (var "countByName"))
                         (Lists.cons (var "newField") (var "done")))]]) $
    -- fold with initial state (empty map, empty list)
    "result" <~ Lists.foldl (var "rewriteCase")
                  (pair (Maps.empty :: TTerm (M.Map Name I.Int32)) (list ([] :: [TTerm Field])))
                  (var "cases_") $
    Lists.reverse (Pairs.second $ var "result")

-- | For unit variants, the case body may contain references to the lambda
--   parameter `v` in applications like `(lambda _ -> innerBody) v`. Since
--   we don't capture `v` in the pattern (unit variants have no value), we
--   need to eliminate all references to `v`. We do this by substituting
--   `v` with `unit` throughout the body, since `v` is only used as an
--   argument to lambdas that ignore their parameter.
eliminateUnitVar :: TBinding (Name -> Term -> Term)
eliminateUnitVar = def "eliminateUnitVar" $
  doc "Substitute unit for a variable in a term (for unit variant case handling)" $
  "v" ~> "term0" ~>
    -- Helper functions that use rewrite
    "rewriteField" <~ ("rewrite" ~> "fld" ~>
      Core.field (Core.fieldName $ var "fld")
                 (var "rewrite" @@ Core.fieldTerm (var "fld"))) $
    "rewriteBinding" <~ ("rewrite" ~> "bnd" ~>
      Core.binding (Core.bindingName $ var "bnd")
                   (var "rewrite" @@ Core.bindingTerm (var "bnd"))
                   (Core.bindingType $ var "bnd")) $
    -- Main rewrite function as Y combinator style
    "rewrite" <~ ("recurse" ~> "term" ~>
      cases _Term (Rewriting.deannotateTerm @@ var "term") (Just $ var "term") [
        -- Replace the variable with unit
        _Term_variable>>: "n" ~>
          Logic.ifElse (Equality.equal (var "n") (var "v"))
            Core.termUnit
            (var "term"),
        -- Recursively rewrite subterms
        _Term_annotated>>: "at" ~>
          Core.termAnnotated $ Core.annotatedTerm
            (var "recurse" @@ Core.annotatedTermBody (var "at"))
            (Core.annotatedTermAnnotation $ var "at"),
        _Term_application>>: "app" ~>
          Core.termApplication $ Core.application
            (var "recurse" @@ Core.applicationFunction (var "app"))
            (var "recurse" @@ Core.applicationArgument (var "app")),
        _Term_function>>: "f" ~>
          cases _Function (var "f") (Just $ var "term") [
            _Function_lambda>>: "lam" ~>
              -- Don't descend if the lambda shadows our variable
              Logic.ifElse (Equality.equal (Core.lambdaParameter $ var "lam") (var "v"))
                (var "term")
                (Core.termFunction $ Core.functionLambda $ Core.lambda
                  (Core.lambdaParameter $ var "lam")
                  (Core.lambdaDomain $ var "lam")
                  (var "recurse" @@ Core.lambdaBody (var "lam"))),
            _Function_elimination>>: "e" ~>
              cases _Elimination (var "e") (Just $ var "term") [
                _Elimination_union>>: "cs" ~>
                  Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
                    Core.caseStatement
                      (Core.caseStatementTypeName $ var "cs")
                      (Maybes.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
                      (Lists.map (var "rewriteField" @@ var "recurse") (Core.caseStatementCases $ var "cs"))]],
        _Term_let>>: "lt" ~>
          Core.termLet $ Core.let_
            (Lists.map (var "rewriteBinding" @@ var "recurse") (Core.letBindings $ var "lt"))
            (var "recurse" @@ Core.letBody (var "lt")),
        _Term_list>>: "ts" ~>
          Core.termList $ Lists.map (var "recurse") (var "ts"),
        _Term_map>>: "m" ~>
          Core.termMap $ Maps.fromList $ Lists.map
            ("kv" ~> pair (var "recurse" @@ Pairs.first (var "kv"))
                         (var "recurse" @@ Pairs.second (var "kv")))
            (Maps.toList $ var "m"),
        _Term_record>>: "rec" ~>
          Core.termRecord $ Core.record
            (Core.recordTypeName $ var "rec")
            (Lists.map (var "rewriteField" @@ var "recurse") (Core.recordFields $ var "rec")),
        _Term_set>>: "s" ~>
          Core.termSet $ Sets.map (var "recurse") (var "s"),
        _Term_union>>: "inj" ~>
          Core.termUnion $ Core.injection
            (Core.injectionTypeName $ var "inj")
            (var "rewriteField" @@ var "recurse" @@ Core.injectionField (var "inj")),
        _Term_maybe>>: "mt" ~>
          Core.termMaybe $ Maybes.map (var "recurse") (var "mt"),
        _Term_pair>>: "p" ~>
          Core.termPair $ pair
            (var "recurse" @@ Pairs.first (var "p"))
            (var "recurse" @@ Pairs.second (var "p")),
        _Term_wrap>>: "wt" ~>
          Core.termWrap $ Core.wrappedTerm
            (Core.wrappedTermTypeName $ var "wt")
            (var "recurse" @@ Core.wrappedTermBody (var "wt")),
        _Term_either>>: "e" ~>
          Core.termEither $ Eithers.bimap (var "recurse") (var "recurse") (var "e"),
        _Term_typeApplication>>: "ta" ~>
          Core.termTypeApplication $ Core.typeApplicationTerm
            (var "recurse" @@ Core.typeApplicationTermBody (var "ta"))
            (Core.typeApplicationTermType $ var "ta"),
        _Term_typeLambda>>: "tl" ~>
          Core.termTypeLambda $ Core.typeLambda
            (Core.typeLambdaParameter $ var "tl")
            (var "recurse" @@ Core.typeLambdaBody (var "tl"))]) $
    -- Fixed point: apply rewrite to itself
    "go" <~ ("term" ~> var "rewrite" @@ var "go" @@ var "term") $
    var "go" @@ var "term0"

-- | Encode the default (wildcard) case block for a match statement.
--   Takes: encoder function, isFull (whether all variants are covered), optional default term, type name
--   Returns: list of CaseBlocks (empty or containing the wildcard case)
--   The encoder function is passed in to allow calling from Staging code that provides encodeTermInline.
encodeDefaultCaseBlock :: TBinding ((Term -> Flow PyHelpers.PyGraph Py.Expression) -> Bool -> Maybe Term -> Name -> Flow PyHelpers.PyGraph [Py.CaseBlock])
encodeDefaultCaseBlock = def "encodeDefaultCaseBlock" $
  doc "Encode the default (wildcard) case block for a match statement" $
  "encodeTerm" ~> "isFull" ~> "mdflt" ~> "tname" ~>
    "stmt" <<~ optCases (var "mdflt")
      -- No default provided
      (produce $ Logic.ifElse (var "isFull")
        (PyUtils.raiseAssertionError @@ string "Unreachable: all variants handled")
        (PyUtils.raiseTypeError @@ (Strings.cat2 (string "Unsupported ") (Names.localNameOf @@ var "tname"))))
      -- Default term provided - encode it as a return statement
      ("d" ~>
        "pyexpr" <<~ (var "encodeTerm" @@ var "d") $
        produce $ PyUtils.returnSingle @@ var "pyexpr") $
    "patterns" <~ (PyUtils.pyClosedPatternToPyPatterns @@ PyDsl.closedPatternWildcard) $
    "body" <~ (PyUtils.indentedBlock @@ nothing @@ list [list [var "stmt"]]) $
    produce $ list [PyDsl.caseBlock (var "patterns") nothing (var "body")]

-- | Encode a single case (Field) into a CaseBlock for a match statement.
--   This handles both enum variants and class-based variants with value capture.
--   The encodeBody function is passed in to allow different encoding strategies
--   (inline vs multiline).
--   Uses withLambda to extend TypeContext with the case binding variable.
encodeCaseBlock :: TBinding (PyHelpers.PythonEnvironment -> Name -> RowType -> Bool -> (PyHelpers.PythonEnvironment -> Term -> Flow PyHelpers.PyGraph [Py.Statement]) -> Field -> Flow PyHelpers.PyGraph Py.CaseBlock)
encodeCaseBlock = def "encodeCaseBlock" $
  doc "Encode a single case (Field) into a CaseBlock for a match statement" $
  "env" ~> "tname" ~> "rowType" ~> "isEnum" ~> "encodeBody" ~> "field" ~>
    "fname" <~ Core.fieldName (var "field") $
    "fterm" <~ Core.fieldTerm (var "field") $
    -- The field term should be a lambda; extract its parameter and body
    cases _Term (Rewriting.deannotateTerm @@ var "fterm") Nothing [
      _Term_function>>: "f" ~>
        cases _Function (var "f") Nothing [
          _Function_lambda>>: "lam" ~>
            -- Extract lambda components
            "v" <~ Core.lambdaParameter (var "lam") $
            "rawBody" <~ Core.lambdaBody (var "lam") $
            -- Check if this variant has unit type
            "isUnitVariant" <~ (isVariantUnitType @@ var "rowType" @@ var "fname") $
            -- For unit variants, eliminate references to the lambda parameter
            "effectiveBody" <~ (Logic.ifElse (var "isUnitVariant")
              (eliminateUnitVar @@ var "v" @@ var "rawBody")
              (var "rawBody")) $
            -- Determine if we should capture the value
            -- Don't capture if: unit variant, variable is free in body, or body is unit term
            "shouldCapture" <~ (Logic.not $ Logic.or (var "isUnitVariant")
              (Logic.or (Rewriting.isFreeVariableInTerm @@ var "v" @@ var "rawBody")
                        (Schemas.isUnitTerm @@ var "rawBody"))) $
            -- Extend the TypeContext with the lambda parameter inside a Flows.bind
            -- to prevent the code generator from reducing it away
            "env2" <<~ (Flows.pure $ pythonEnvironmentSetTypeContext
              @@ (Schemas.extendTypeContextForLambda @@ (pythonEnvironmentGetTypeContext @@ var "env") @@ var "lam")
              @@ var "env") $
            -- Create the pattern using env2 (extended context)
            "pattern" <~ (variantClosedPattern @@ var "env2" @@ var "tname" @@ var "fname"
              @@ var "rowType" @@ var "isEnum" @@ var "v" @@ var "shouldCapture") $
            -- Encode the body using the provided encoder with extended env
            "stmts" <<~ (var "encodeBody" @@ var "env2" @@ var "effectiveBody") $
            "pyBody" <~ (PyUtils.indentedBlock @@ nothing @@ list [var "stmts"]) $
            produce $ PyDsl.caseBlock
              (PyUtils.pyClosedPatternToPyPatterns @@ var "pattern")
              nothing
              (var "pyBody")]]

-- | Accessor for the graph field of PyGraph
pyGraphGraph :: TBinding (PyHelpers.PyGraph -> Graph)
pyGraphGraph = def "pyGraphGraph" $
  doc "Accessor for the graph field of PyGraph" $
  Phantoms.project PyHelpers._PyGraph PyHelpers._PyGraph_graph

-- | Accessor for the metadata field of PyGraph
pyGraphMetadata :: TBinding (PyHelpers.PyGraph -> PyHelpers.PythonModuleMetadata)
pyGraphMetadata = def "pyGraphMetadata" $
  doc "Accessor for the metadata field of PyGraph" $
  Phantoms.project PyHelpers._PyGraph PyHelpers._PyGraph_metadata

-- | Constructor for PyGraph record
makePyGraph :: TBinding (Graph -> PyHelpers.PythonModuleMetadata -> PyHelpers.PyGraph)
makePyGraph = def "makePyGraph" $
  doc "Constructor for PyGraph record" $
  "g" ~> "m" ~>
    Phantoms.record PyHelpers._PyGraph [
      Phantoms.field PyHelpers._PyGraph_graph (var "g"),
      Phantoms.field PyHelpers._PyGraph_metadata (var "m")]

-- | Run a Flow Graph computation within a Flow PyGraph computation
inGraphContext :: TBinding (Flow Graph a -> Flow PyHelpers.PyGraph a)
inGraphContext = def "inGraphContext" $
  doc "Run a Flow Graph computation within a Flow PyGraph computation" $
  "graphFlow" ~>
    CoderUtils.inCoderGraphContext @@
      pyGraphGraph @@
      pyGraphMetadata @@
      makePyGraph @@
      var "graphFlow"

-- | Encode a field type for record definitions (field: type annotation)
encodeFieldType :: TBinding (PyHelpers.PythonEnvironment -> FieldType -> Flow PyHelpers.PyGraph Py.Statement)
encodeFieldType = def "encodeFieldType" $
  doc "Encode a field type for record definitions (field: type annotation)" $
  "env" ~> "fieldType" ~>
    "fname" <~ Core.fieldTypeName (var "fieldType") $
    "ftype" <~ Core.fieldTypeType (var "fieldType") $
    "comment" <<~ (inGraphContext @@ (Annotations.getTypeDescription @@ var "ftype")) $
    "pyName" <~ (PyDsl.singleTargetName $ PyNames.encodeFieldName @@ var "env" @@ var "fname") $
    "pyType" <<~ (encodeType @@ var "env" @@ var "ftype") $
    "annotatedPyType" <~ (PyUtils.annotatedExpression @@ var "comment" @@ var "pyType") $
    produce $ PyUtils.pyAssignmentToPyStatement @@
      (PyDsl.assignmentTyped $ PyDsl.typedAssignment (var "pyName") (var "annotatedPyType") nothing)

-- | Create a @dataclass(frozen=True) decorator
dataclassDecorator :: TBinding Py.NamedExpression
dataclassDecorator = def "dataclassDecorator" $
  doc "Create a @dataclass(frozen=True) decorator" $
  PyDsl.namedExpressionSimple $
    PyUtils.pyPrimaryToPyExpression @@
      (PyUtils.primaryWithRhs @@
        (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "dataclass") @@
        (PyDsl.primaryRhsCall $ PyDsl.args
          (Phantoms.list ([] :: [TTerm Py.PosArg]))
          (Phantoms.list [PyDsl.kwargOrStarredKwarg $ PyDsl.kwarg (PyDsl.name $ string "frozen") (PyUtils.pyAtomToPyExpression @@ PyDsl.atomTrue)])
          (Phantoms.list ([] :: [TTerm Py.KwargOrDoubleStarred]))))

-- | Encode a record type as a Python dataclass
encodeRecordType :: TBinding (PyHelpers.PythonEnvironment -> Name -> RowType -> Maybe String -> Flow PyHelpers.PyGraph Py.Statement)
encodeRecordType = def "encodeRecordType" $
  doc "Encode a record type as a Python dataclass" $
  "env" ~> "name" ~> "rowType" ~> "comment" ~>
    "tfields" <~ Core.rowTypeFields (var "rowType") $
    "pyFields" <<~ (Flows.mapList (encodeFieldType @@ var "env") (var "tfields")) $
    "body" <~ (PyUtils.indentedBlock @@ var "comment" @@ list [var "pyFields"]) $
    -- Get bound type variables for Generic args
    "boundVars" <~ (Phantoms.project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env") $
    "tparamList" <~ Pairs.first (var "boundVars") $
    "mGenericArg" <~ (genericArg @@ var "tparamList") $
    "args" <~ optCases (var "mGenericArg")
      nothing
      ("a" ~> just (PyUtils.pyExpressionsToPyArgs @@ list [var "a"])) $
    "decs" <~ (just $ wrap Py._Decorators $ list [dataclassDecorator]) $
    "pyName" <~ (PyNames.encodeName @@ Phantoms.false @@ Util.caseConventionPascal @@ var "env" @@ var "name") $
    "noTypeParams" <~ (Phantoms.list ([] :: [TTerm Py.TypeParameter])) $
    produce $ PyUtils.pyClassDefinitionToPyStatement @@
      Phantoms.record Py._ClassDefinition [
        Phantoms.field Py._ClassDefinition_decorators (var "decs"),
        Phantoms.field Py._ClassDefinition_name (var "pyName"),
        Phantoms.field Py._ClassDefinition_typeParams (var "noTypeParams"),
        Phantoms.field Py._ClassDefinition_arguments (var "args"),
        Phantoms.field Py._ClassDefinition_body (var "body")]

-- | Encode an enum value assignment: ENUM_VALUE = "enum_value"
encodeEnumValueAssignment :: TBinding (PyHelpers.PythonEnvironment -> FieldType -> Flow PyHelpers.PyGraph [Py.Statement])
encodeEnumValueAssignment = def "encodeEnumValueAssignment" $
  doc "Encode an enum value assignment statement with optional comment" $
  "env" ~> "fieldType" ~>
    "fname" <~ Core.fieldTypeName (var "fieldType") $
    "ftype" <~ Core.fieldTypeType (var "fieldType") $
    "mcomment" <<~ (inGraphContext @@ (Annotations.getTypeDescription @@ var "ftype")) $
    "pyName" <~ (PyNames.encodeEnumValue @@ var "env" @@ var "fname") $
    "fnameStr" <~ (Core.unName $ var "fname") $
    "pyValue" <~ (PyUtils.doubleQuotedString @@ var "fnameStr") $
    "assignStmt" <~ (PyUtils.assignmentStatement @@ var "pyName" @@ var "pyValue") $
    produce $ optCases (var "mcomment")
      (list [var "assignStmt"])
      ("c" ~> list [var "assignStmt", PyUtils.pyExpressionToPyStatement @@ (PyUtils.tripleQuotedString @@ var "c")])

-- | Encode a union field as a variant class
encodeUnionField :: TBinding (PyHelpers.PythonEnvironment -> Name -> FieldType -> Flow PyHelpers.PyGraph Py.Statement)
encodeUnionField = def "encodeUnionField" $
  doc "Encode a union field as a variant class" $
  "env" ~> "unionName" ~> "fieldType" ~>
    "fname" <~ Core.fieldTypeName (var "fieldType") $
    "ftype" <~ Core.fieldTypeType (var "fieldType") $
    "fcomment" <<~ (inGraphContext @@ (Annotations.getTypeDescription @@ var "ftype")) $
    "isUnit" <~ (Equality.equal (Rewriting.deannotateType @@ var "ftype") (Core.typeUnit)) $
    "varName" <~ (PyNames.variantName @@ false @@ var "env" @@ var "unionName" @@ var "fname") $
    "tparamNames" <~ (findTypeParams @@ var "env" @@ var "ftype") $
    "tparamPyNames" <~ Lists.map PyNames.encodeTypeVariable (var "tparamNames") $
    "fieldParams" <~ Lists.map PyUtils.pyNameToPyTypeParameter (var "tparamPyNames") $
    -- For unit types, use unitVariantMethods for body; otherwise empty
    "body" <~ Logic.ifElse (var "isUnit")
      (PyUtils.indentedBlock @@ var "fcomment" @@ list [PyUtils.unitVariantMethods @@ var "varName"])
      (PyUtils.indentedBlock @@ var "fcomment" @@ Phantoms.list ([] :: [TTerm [Py.Statement]])) $
    -- For unit types, no args; otherwise variantArgs
    "margs" <<~ Logic.ifElse (var "isUnit")
      (Flows.pure (nothing :: TTerm (Maybe Py.Args)))
      ("quotedType" <<~ (encodeTypeQuoted @@ var "env" @@ var "ftype") $
       produce $ just (variantArgs @@ var "quotedType" @@ Phantoms.list ([] :: [TTerm Name]))) $
    produce $ PyUtils.pyClassDefinitionToPyStatement @@
      PyDsl.classDefinition
        nothing
        (var "varName")
        (var "fieldParams")
        (var "margs")
        (var "body")

-- | Encode a union type as either an enum or a set of variant classes
encodeUnionType :: TBinding (PyHelpers.PythonEnvironment -> Name -> RowType -> Maybe String -> Flow PyHelpers.PyGraph [Py.Statement])
encodeUnionType = def "encodeUnionType" $
  doc "Encode a union type as an enum (for unit-only fields) or variant classes" $
  "env" ~> "name" ~> "rowType" ~> "comment" ~>
    "tfields" <~ Core.rowTypeFields (var "rowType") $
    Logic.ifElse (Schemas.isEnumRowType @@ var "rowType")
      -- Enum case
      ("vals" <<~ (Flows.mapList (encodeEnumValueAssignment @@ var "env") (var "tfields")) $
       "body" <~ (PyUtils.indentedBlock @@ var "comment" @@ var "vals") $
       "enumName" <~ PyDsl.name (string "Enum") $
       "args" <~ (just $ PyUtils.pyExpressionsToPyArgs @@ list [PyUtils.pyNameToPyExpression @@ var "enumName"]) $
       "pyName" <~ (PyNames.encodeName @@ false @@ Util.caseConventionPascal @@ var "env" @@ var "name") $
       produce $ list [PyUtils.pyClassDefinitionToPyStatement @@
         PyDsl.classDefinition nothing (var "pyName") (Phantoms.list ([] :: [TTerm Py.TypeParameter])) (var "args") (var "body")])
      -- Union case
      ("fieldStmts" <<~ (Flows.mapList (encodeUnionField @@ var "env" @@ var "name") (var "tfields")) $
       "tparams" <~ (environmentTypeParameters @@ var "env") $
       "unionAlts" <~ Lists.map (encodeUnionFieldAlt @@ var "env" @@ var "name") (var "tfields") $
       "unionStmts" <~ (unionTypeStatementsFor @@ var "env" @@
         (PyNames.encodeName @@ false @@ Util.caseConventionPascal @@ var "env" @@ var "name") @@
         (var "tparams") @@
         (var "comment") @@
         (PyUtils.orExpression @@ var "unionAlts")) $
       produce $ Lists.concat2 (var "fieldStmts") (var "unionStmts"))

-- | Encode a union field as an alternative expression for the union type alias
encodeUnionFieldAlt :: TBinding (PyHelpers.PythonEnvironment -> Name -> FieldType -> Py.Primary)
encodeUnionFieldAlt = def "encodeUnionFieldAlt" $
  doc "Encode a union field as a primary expression for | alternatives" $
  "env" ~> "unionName" ~> "fieldType" ~>
    "fname" <~ Core.fieldTypeName (var "fieldType") $
    "ftype" <~ Core.fieldTypeType (var "fieldType") $
    "tparamNames" <~ (findTypeParams @@ var "env" @@ var "ftype") $
    "tparams" <~ Lists.map PyNames.encodeTypeVariable (var "tparamNames") $
    "namePrim" <~ (PyUtils.pyNameToPyPrimary @@ (PyNames.variantName @@ false @@ var "env" @@ var "unionName" @@ var "fname")) $
    Logic.ifElse (Lists.null (var "tparams"))
      (var "namePrim")
      ("tparamExprs" <~ Lists.map PyUtils.pyNameToPyExpression (var "tparams") $
       PyUtils.primaryWithExpressionSlices @@ var "namePrim" @@ var "tparamExprs")

-- | Encode a type definition with a single statement result
encodeTypeDefSingle :: TBinding (PyHelpers.PythonEnvironment -> Name -> Maybe String -> Py.Expression -> [Py.Statement])
encodeTypeDefSingle = def "encodeTypeDefSingle" $
  doc "Encode a simple type alias definition" $
  "env" ~> "name" ~> "comment" ~> "typeExpr" ~>
    "pyName" <~ (PyNames.encodeName @@ false @@ Util.caseConventionPascal @@ var "env" @@ var "name") $
    "tparams" <~ (environmentTypeParameters @@ var "env") $
    list [typeAliasStatementFor @@ var "env" @@ var "pyName" @@ var "tparams" @@ var "comment" @@ var "typeExpr"]

-- | Encode a type assignment (dispatches to record, union, wrap, or simple typedef)
encodeTypeAssignment :: TBinding (PyHelpers.PythonEnvironment -> Name -> Type -> Maybe String -> Flow PyHelpers.PyGraph [[Py.Statement]])
encodeTypeAssignment = def "encodeTypeAssignment" $
  doc "Encode a type definition, dispatching based on type structure" $
  "env" ~> "name" ~> "typ" ~> "comment" ~>
    "defStmts" <<~ (encodeTypeAssignmentInner @@ var "env" @@ var "name" @@ var "typ" @@ var "comment") $
    "constStmts" <~ (encodeNameConstants @@ var "env" @@ var "name" @@ var "typ") $
    produce $ Lists.concat2
      (Lists.map ("s" ~> list [var "s"]) (var "defStmts"))
      (list [var "constStmts"])

-- | Inner type assignment encoding that handles forall unwrapping
encodeTypeAssignmentInner :: TBinding (PyHelpers.PythonEnvironment -> Name -> Type -> Maybe String -> Flow PyHelpers.PyGraph [Py.Statement])
encodeTypeAssignmentInner = def "encodeTypeAssignmentInner" $
  doc "Encode the inner type definition, unwrapping forall types" $
  "env" ~> "name" ~> "typ" ~> "comment" ~>
    "stripped" <~ (Rewriting.deannotateType @@ var "typ") $
    -- Default: simple type alias
    "dflt" <~ ("typeExpr" <<~ (encodeType @@ var "env" @@ var "typ") $
       produce $ encodeTypeDefSingle @@ var "env" @@ var "name" @@ var "comment" @@ var "typeExpr") $
    cases _Type (var "stripped") (Just (var "dflt")) [
      -- Forall: extend environment with type variable and recurse
      _Type_forall>>: "ft" ~>
        "tvar" <~ (Core.forallTypeParameter $ var "ft") $
        "body" <~ (Core.forallTypeBody $ var "ft") $
        "newEnv" <~ (extendEnvWithTypeVar @@ var "env" @@ var "tvar") $
        encodeTypeAssignmentInner @@ var "newEnv" @@ var "name" @@ var "body" @@ var "comment",

      -- Record type
      _Type_record>>: "rt" ~>
        Flows.map ("s" ~> list [var "s"]) (encodeRecordType @@ var "env" @@ var "name" @@ var "rt" @@ var "comment"),

      -- Union type
      _Type_union>>: "rt" ~>
        encodeUnionType @@ var "env" @@ var "name" @@ var "rt" @@ var "comment",

      -- Wrapped type
      _Type_wrap>>: "wt" ~>
        "innerType" <~ (Core.wrappedTypeBody $ var "wt") $
        Flows.map ("s" ~> list [var "s"]) (encodeWrappedType @@ var "env" @@ var "name" @@ var "innerType" @@ var "comment")]

-- | Encode a field (name-value pair) to a Python (Name, Expression) pair
encodeField :: TBinding (PyHelpers.PythonEnvironment -> Field -> (TTerm Term -> Flow PyHelpers.PyGraph Py.Expression) -> Flow PyHelpers.PyGraph (Py.Name, Py.Expression))
encodeField = def "encodeField" $
  doc "Encode a field (name-value pair) to a Python (Name, Expression) pair" $
  "env" ~> "field" ~> "encodeTerm" ~>
    "fname" <~ Core.fieldName (var "field") $
    "fterm" <~ Core.fieldTerm (var "field") $
    "pterm" <<~ (var "encodeTerm" @@ var "fterm") $
    produce $ pair (PyNames.encodeFieldName @@ var "env" @@ var "fname") (var "pterm")

-- | Encode bindings as function definitions
encodeBindingsAsDefs :: TBinding (PyHelpers.PythonEnvironment -> (PyHelpers.PythonEnvironment -> Binding -> Flow PyHelpers.PyGraph Py.Statement) -> [Binding] -> Flow PyHelpers.PyGraph [Py.Statement])
encodeBindingsAsDefs = def "encodeBindingsAsDefs" $
  doc "Encode bindings as function definitions" $
  "env" ~> "encodeBinding" ~> "bindings" ~>
    Flows.mapList (var "encodeBinding" @@ var "env") (var "bindings")

-- | Encode a single binding as a Python statement
--   This handles:
--   1. Bindings with type schemes: use encodeTermAssignment
--   2. Hoisted bindings: lambdas wrapping a case statement application (from hoisting)
--   3. Case elimination functions: case statements as values
--   4. Other terms: falls back to encodeTermMultiline
encodeBindingAs :: TBinding (PyHelpers.PythonEnvironment -> Binding -> Flow PyHelpers.PyGraph Py.Statement)
encodeBindingAs = def "encodeBindingAs" $
  doc "Encode a binding as a Python statement (function definition or assignment)" $
  "env" ~> "binding" ~>
    "name1" <~ Core.bindingName (var "binding") $
    "term1" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    "fname" <~ (PyNames.encodeName @@ true @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name1") $
    -- Check if binding has a type scheme - if so, use encodeTermAssignment
    Maybes.maybe
      -- No type scheme (e.g., lifted local functions) - check for special patterns
      ("gathered" <~ (gatherLambdas @@ var "term1") $
        "lambdaParams" <~ (Pairs.first $ var "gathered") $
        "innerBody" <~ (Pairs.second $ var "gathered") $
        -- Check for hoisted binding pattern: lambdas wrapping a case statement application
        "mcsa" <~ (isCaseStatementApplication @@ var "innerBody") $
        -- Try hoisted binding pattern first
        Maybes.maybe
          -- Not a hoisted binding, try simple case elimination
          ("mcs" <~ (extractCaseElimination @@ var "term1") $
            Maybes.maybe
              -- Default case: not a case elimination, encode term normally and take first statement
              (Flows.map ("stmts" ~> Lists.head (var "stmts")) (encodeTermMultiline @@ var "env" @@ var "term1"))
              -- Case elimination function - encode as function with match statement
          ("cs" ~>
            "tname" <~ (Core.caseStatementTypeName $ var "cs") $
            "dflt" <~ (Core.caseStatementDefault $ var "cs") $
            "cases_" <~ (Core.caseStatementCases $ var "cs") $
            "rt" <<~ (inGraphContext @@ (Schemas.requireUnionType @@ var "tname")) $
            "isEnum" <~ (Schemas.isEnumRowType @@ var "rt") $
            "isFull" <~ (isCasesFull @@ var "rt" @@ var "cases_") $
            "innerParam" <~ (PyDsl.param (PyDsl.name $ string "x") nothing) $
            "param" <~ (Phantoms.record Py._ParamNoDefault [
              Phantoms.field Py._ParamNoDefault_param (var "innerParam"),
              Phantoms.field Py._ParamNoDefault_typeComment nothing]) $
            "params" <~ (PyDsl.parametersParamNoDefault $ Phantoms.record Py._ParamNoDefaultParameters [
              Phantoms.field Py._ParamNoDefaultParameters_paramNoDefault (list [var "param"]),
              Phantoms.field Py._ParamNoDefaultParameters_paramWithDefault (Phantoms.list ([] :: [TTerm Py.ParamWithDefault])),
              Phantoms.field Py._ParamNoDefaultParameters_starEtc nothing]) $
            "pyCases" <<~ (Flows.mapList (encodeCaseBlock @@ var "env" @@ var "tname" @@ var "rt" @@ var "isEnum" @@ ("e" ~> "t" ~> encodeTermMultiline @@ var "e" @@ var "t")) (var "cases_")) $
            "pyDflt" <<~ (encodeDefaultCaseBlock @@ ("t" ~> encodeTermInline @@ var "env" @@ false @@ var "t") @@ var "isFull" @@ var "dflt" @@ var "tname") $
            "subj" <~ (PyDsl.subjectExpressionSimple $ PyDsl.namedExpressionSimple $ PyUtils.pyNameToPyExpression @@ (PyDsl.name $ string "x")) $
            "allCases" <~ (Lists.concat2 (var "pyCases") (var "pyDflt")) $
            "matchStmt" <~ (PyDsl.statementCompound $ PyDsl.compoundStatementMatch $ Phantoms.record Py._MatchStatement [
              Phantoms.field Py._MatchStatement_subject (var "subj"),
              Phantoms.field Py._MatchStatement_cases (var "allCases")]) $
            "body" <~ (PyUtils.indentedBlock @@ nothing @@ list [list [var "matchStmt"]]) $
            "funcDefRaw" <~ (Phantoms.record Py._FunctionDefRaw [
              Phantoms.field Py._FunctionDefRaw_async false,
              Phantoms.field Py._FunctionDefRaw_name (var "fname"),
              Phantoms.field Py._FunctionDefRaw_typeParams (Phantoms.list ([] :: [TTerm Py.TypeParameter])),
              Phantoms.field Py._FunctionDefRaw_params (just $ var "params"),
              Phantoms.field Py._FunctionDefRaw_returnType nothing,
              Phantoms.field Py._FunctionDefRaw_funcTypeComment nothing,
              Phantoms.field Py._FunctionDefRaw_block (var "body")]) $
            produce $ PyDsl.statementCompound (PyDsl.compoundStatementFunction $ PyDsl.functionDefinition nothing (var "funcDefRaw")))
          (var "mcs"))
      -- Hoisted binding: lambdas wrapping a case statement application
      -- Only handle if there are actual lambda parameters
      ("csa" ~>
        Logic.ifElse (Lists.null $ var "lambdaParams")
          -- No lambda params, fall back to case elimination check
          ("mcs" <~ (extractCaseElimination @@ var "term1") $
            Maybes.maybe
              (Flows.map ("stmts" ~> Lists.head (var "stmts")) (encodeTermMultiline @@ var "env" @@ var "term1"))
              ("cs" ~>
                "tname" <~ (Core.caseStatementTypeName $ var "cs") $
                "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                "cases_" <~ (Core.caseStatementCases $ var "cs") $
                "rt" <<~ (inGraphContext @@ (Schemas.requireUnionType @@ var "tname")) $
                "isEnum" <~ (Schemas.isEnumRowType @@ var "rt") $
                "isFull" <~ (isCasesFull @@ var "rt" @@ var "cases_") $
                "innerParam" <~ (PyDsl.param (PyDsl.name $ string "x") nothing) $
                "param" <~ (Phantoms.record Py._ParamNoDefault [
                  Phantoms.field Py._ParamNoDefault_param (var "innerParam"),
                  Phantoms.field Py._ParamNoDefault_typeComment nothing]) $
                "params" <~ (PyDsl.parametersParamNoDefault $ Phantoms.record Py._ParamNoDefaultParameters [
                  Phantoms.field Py._ParamNoDefaultParameters_paramNoDefault (list [var "param"]),
                  Phantoms.field Py._ParamNoDefaultParameters_paramWithDefault (Phantoms.list ([] :: [TTerm Py.ParamWithDefault])),
                  Phantoms.field Py._ParamNoDefaultParameters_starEtc nothing]) $
                "pyCases" <<~ (Flows.mapList (encodeCaseBlock @@ var "env" @@ var "tname" @@ var "rt" @@ var "isEnum" @@ ("e" ~> "t" ~> encodeTermMultiline @@ var "e" @@ var "t")) (var "cases_")) $
                "pyDflt" <<~ (encodeDefaultCaseBlock @@ ("t" ~> encodeTermInline @@ var "env" @@ false @@ var "t") @@ var "isFull" @@ var "dflt" @@ var "tname") $
                "subj" <~ (PyDsl.subjectExpressionSimple $ PyDsl.namedExpressionSimple $ PyUtils.pyNameToPyExpression @@ (PyDsl.name $ string "x")) $
                "allCases" <~ (Lists.concat2 (var "pyCases") (var "pyDflt")) $
                "matchStmt" <~ (PyDsl.statementCompound $ PyDsl.compoundStatementMatch $ Phantoms.record Py._MatchStatement [
                  Phantoms.field Py._MatchStatement_subject (var "subj"),
                  Phantoms.field Py._MatchStatement_cases (var "allCases")]) $
                "body" <~ (PyUtils.indentedBlock @@ nothing @@ list [list [var "matchStmt"]]) $
                "funcDefRaw" <~ (Phantoms.record Py._FunctionDefRaw [
                  Phantoms.field Py._FunctionDefRaw_async false,
                  Phantoms.field Py._FunctionDefRaw_name (var "fname"),
                  Phantoms.field Py._FunctionDefRaw_typeParams (Phantoms.list ([] :: [TTerm Py.TypeParameter])),
                  Phantoms.field Py._FunctionDefRaw_params (just $ var "params"),
                  Phantoms.field Py._FunctionDefRaw_returnType nothing,
                  Phantoms.field Py._FunctionDefRaw_funcTypeComment nothing,
                  Phantoms.field Py._FunctionDefRaw_block (var "body")]) $
                produce $ PyDsl.statementCompound (PyDsl.compoundStatementFunction $ PyDsl.functionDefinition nothing (var "funcDefRaw")))
              (var "mcs"))
          -- Has lambda params: this is a hoisted binding
          -- Encode as: def fname(lambdaParams..., matchArg): match matchArg: ...
          -- Extract components from the nested tuple: (tname, (dflt, (cases, arg)))
          ("tname" <~ (Pairs.first $ var "csa") $
            "rest1" <~ (Pairs.second $ var "csa") $
            "dflt" <~ (Pairs.first $ var "rest1") $
            "rest2" <~ (Pairs.second $ var "rest1") $
            "cases_" <~ (Pairs.first $ var "rest2") $
            "rt" <<~ (inGraphContext @@ (Schemas.requireUnionType @@ var "tname")) $
            "isEnum" <~ (Schemas.isEnumRowType @@ var "rt") $
            "isFull" <~ (isCasesFull @@ var "rt" @@ var "cases_") $
            -- Create parameters for captured variables
            "capturedParams" <~ (Lists.map
              ("n" ~> Phantoms.record Py._ParamNoDefault [
                Phantoms.field Py._ParamNoDefault_param (PyDsl.param
                  (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "n")
                  nothing),
                Phantoms.field Py._ParamNoDefault_typeComment nothing])
              (var "lambdaParams")) $
            -- Create the match argument parameter
            "matchArgName" <~ (PyDsl.name $ string "x") $
            "matchParam" <~ (Phantoms.record Py._ParamNoDefault [
              Phantoms.field Py._ParamNoDefault_param (PyDsl.param (var "matchArgName") nothing),
              Phantoms.field Py._ParamNoDefault_typeComment nothing]) $
            "allParams" <~ (Lists.concat2 (var "capturedParams") (list [var "matchParam"])) $
            "params" <~ (PyDsl.parametersParamNoDefault $ Phantoms.record Py._ParamNoDefaultParameters [
              Phantoms.field Py._ParamNoDefaultParameters_paramNoDefault (var "allParams"),
              Phantoms.field Py._ParamNoDefaultParameters_paramWithDefault (Phantoms.list ([] :: [TTerm Py.ParamWithDefault])),
              Phantoms.field Py._ParamNoDefaultParameters_starEtc nothing]) $
            -- Extend environment with all gathered lambda parameters before encoding cases
            "envWithParams" <~ (extendEnvWithLambdaParams @@ var "env" @@ var "term1") $
            -- Encode the match statement using extended environment
            "pyCases" <<~ (Flows.mapList (encodeCaseBlock @@ var "envWithParams" @@ var "tname" @@ var "rt" @@ var "isEnum" @@ ("e" ~> "t" ~> encodeTermMultiline @@ var "e" @@ var "t")) (var "cases_")) $
            "pyDflt" <<~ (encodeDefaultCaseBlock @@ ("t" ~> encodeTermInline @@ var "envWithParams" @@ false @@ var "t") @@ var "isFull" @@ var "dflt" @@ var "tname") $
            "subj" <~ (PyDsl.subjectExpressionSimple $ PyDsl.namedExpressionSimple $ PyUtils.pyNameToPyExpression @@ var "matchArgName") $
            "allCases" <~ (Lists.concat2 (var "pyCases") (var "pyDflt")) $
            "matchStmt" <~ (PyDsl.statementCompound $ PyDsl.compoundStatementMatch $ Phantoms.record Py._MatchStatement [
              Phantoms.field Py._MatchStatement_subject (var "subj"),
              Phantoms.field Py._MatchStatement_cases (var "allCases")]) $
            "body" <~ (PyUtils.indentedBlock @@ nothing @@ list [list [var "matchStmt"]]) $
            "funcDefRaw" <~ (Phantoms.record Py._FunctionDefRaw [
              Phantoms.field Py._FunctionDefRaw_async false,
              Phantoms.field Py._FunctionDefRaw_name (var "fname"),
              Phantoms.field Py._FunctionDefRaw_typeParams (Phantoms.list ([] :: [TTerm Py.TypeParameter])),
              Phantoms.field Py._FunctionDefRaw_params (just $ var "params"),
              Phantoms.field Py._FunctionDefRaw_returnType nothing,
              Phantoms.field Py._FunctionDefRaw_funcTypeComment nothing,
              Phantoms.field Py._FunctionDefRaw_block (var "body")]) $
            produce $ PyDsl.statementCompound (PyDsl.compoundStatementFunction $ PyDsl.functionDefinition nothing (var "funcDefRaw"))))
          (var "mcsa"))
      -- Binding with type scheme - use encodeTermAssignment
      ("ts" ~>
        "comment" <<~ (inGraphContext @@ (Annotations.getTermDescription @@ var "term1")) $
        "normComment" <~ (Maybes.map CoderUtils.normalizeComment (var "comment")) $
        encodeTermAssignment @@ var "env" @@ var "name1" @@ var "term1" @@ var "ts" @@ var "normComment")
      (var "mts")

-- | Encode a definition (term or type) to Python statements
encodeDefinition :: TBinding (PyHelpers.PythonEnvironment
  -> Definition
  -> Flow PyHelpers.PyGraph [[Py.Statement]])
encodeDefinition = def "encodeDefinition" $
  doc "Encode a definition (term or type) to Python statements" $
  "env" ~> "def_" ~>
    cases _Definition (var "def_") Nothing [
      _Definition_term>>: "td" ~>
        "name" <~ (project _TermDefinition _TermDefinition_name @@ var "td") $
        "term" <~ (project _TermDefinition _TermDefinition_term @@ var "td") $
        "typ" <~ (project _TermDefinition _TermDefinition_type @@ var "td") $
        "comment" <<~ (inGraphContext @@ (Annotations.getTermDescription @@ var "term")) $
        "normComment" <~ (Maybes.map CoderUtils.normalizeComment (var "comment")) $
        "stmt" <<~ (encodeTermAssignment @@ var "env" @@ var "name" @@ var "term" @@ var "typ" @@ var "normComment") $
        produce $ list [list [var "stmt"]],
      _Definition_type>>: "td" ~>
        "name" <~ (project _TypeDefinition _TypeDefinition_name @@ var "td") $
        "typ" <~ (project _TypeDefinition _TypeDefinition_type @@ var "td") $
        "comment" <<~ (inGraphContext @@ (Annotations.getTypeDescription @@ var "typ")) $
        "normComment" <~ (Maybes.map CoderUtils.normalizeComment (var "comment")) $
        encodeTypeAssignment @@ var "env" @@ var "name" @@ var "typ" @@ var "normComment"]

-- | Update the Python module metadata in the coder state
updateMeta :: TBinding ((PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata) -> Flow PyHelpers.PyGraph ())
updateMeta = def "updateMeta" $
  doc "Update the Python module metadata in the coder state" $
  CoderUtils.updateCoderMetadata @@
    pyGraphMetadata @@
    makePyGraph @@
    pyGraphGraph

-- | Execute a computation with bindings added to the graph context
withBindings :: TBinding ([Binding] -> Flow PyHelpers.PyGraph a -> Flow PyHelpers.PyGraph a)
withBindings = def "withBindings" $
  doc "Execute a computation with bindings added to the graph context" $
  CoderUtils.withGraphBindings @@
    pyGraphGraph @@
    makePyGraph @@
    pyGraphMetadata

-- | Execute a computation with an updated graph
withUpdatedGraph :: TBinding ((Graph -> Graph) -> Flow PyHelpers.PyGraph a -> Flow PyHelpers.PyGraph a)
withUpdatedGraph = def "withUpdatedGraph" $
  doc "Execute a computation with an updated graph" $
  CoderUtils.withUpdatedCoderGraph @@
    pyGraphGraph @@
    pyGraphMetadata @@
    makePyGraph

-- | Calculate the arity of a term, with proper handling of primitives.
--   Unlike Arity.termArity, this looks up primitive arities from the graph
--   rather than returning a placeholder value.
termArityWithPrimitives :: TBinding (Graph -> Term -> Int)
termArityWithPrimitives = def "termArityWithPrimitives" $
  doc "Calculate term arity with proper primitive handling" $
  "graph" ~> "term" ~>
    cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "term") (Just (Phantoms.int 0)) [
      _Term_application>>: "app" ~>
        Math.max (Phantoms.int 0) (Math.sub
          (termArityWithPrimitives @@ var "graph" @@ (Core.applicationFunction $ var "app"))
          (Phantoms.int 1)),
      _Term_function>>: "f" ~>
        functionArityWithPrimitives @@ var "graph" @@ var "f"]

-- | Calculate the arity of a function, with proper handling of primitives.
functionArityWithPrimitives :: TBinding (Graph -> Function -> Int)
functionArityWithPrimitives = def "functionArityWithPrimitives" $
  doc "Calculate function arity with proper primitive handling" $
  "graph" ~> "f" ~>
    cases _Function (var "f") (Just (Phantoms.int 0)) [
      _Function_elimination>>: constant (Phantoms.int 1),
      _Function_lambda>>: "lam" ~>
        Math.add (Phantoms.int 1) (termArityWithPrimitives @@ var "graph" @@ (Core.lambdaBody $ var "lam")),
      _Function_primitive>>: "name" ~>
        optCases (Maps.lookup (var "name") (Graph.graphPrimitives $ var "graph"))
          (Phantoms.int 0)
          ("prim" ~> Arity.primitiveArity @@ var "prim")]

-- | Get the TypeContext from a PythonEnvironment
pythonEnvironmentGetTypeContext :: TBinding (PyHelpers.PythonEnvironment -> TypeContext)
pythonEnvironmentGetTypeContext = def "pythonEnvironmentGetTypeContext" $
  doc "Get the TypeContext from a PythonEnvironment" $
  Phantoms.project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext

-- | Set the TypeContext in a PythonEnvironment
pythonEnvironmentSetTypeContext :: TBinding (TypeContext -> PyHelpers.PythonEnvironment -> PyHelpers.PythonEnvironment)
pythonEnvironmentSetTypeContext = def "pythonEnvironmentSetTypeContext" $
  doc "Set the TypeContext in a PythonEnvironment" $
  "tc" ~> "env" ~>
    record PyHelpers._PythonEnvironment [
      PyHelpers._PythonEnvironment_namespaces>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "env",
      PyHelpers._PythonEnvironment_boundTypeVariables>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env",
      PyHelpers._PythonEnvironment_typeContext>>: var "tc",
      PyHelpers._PythonEnvironment_nullaryBindings>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_nullaryBindings @@ var "env",
      PyHelpers._PythonEnvironment_version>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_version @@ var "env",
      PyHelpers._PythonEnvironment_skipCasts>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_skipCasts @@ var "env",
      PyHelpers._PythonEnvironment_inlineVariables>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_inlineVariables @@ var "env"]

-- | Execute a computation with lambda context (adds lambda parameter to TypeContext)
withLambda :: TBinding (PyHelpers.PythonEnvironment -> Lambda -> (PyHelpers.PythonEnvironment -> Flow s a) -> Flow s a)
withLambda = def "withLambda" $
  doc "Execute a computation with lambda context (adds lambda parameter to TypeContext)" $
  Schemas.withLambdaContext @@
    pythonEnvironmentGetTypeContext @@
    pythonEnvironmentSetTypeContext

-- | Execute a computation with type lambda context (adds type parameter to TypeContext)
withTypeLambda :: TBinding (PyHelpers.PythonEnvironment -> TypeLambda -> (PyHelpers.PythonEnvironment -> Flow s a) -> Flow s a)
withTypeLambda = def "withTypeLambda" $
  doc "Execute a computation with type lambda context" $
  Schemas.withTypeLambdaContext @@
    pythonEnvironmentGetTypeContext @@
    pythonEnvironmentSetTypeContext

-- | Execute a computation with let context (adds let bindings to TypeContext with metadata)
withLet :: TBinding (PyHelpers.PythonEnvironment -> Let -> (PyHelpers.PythonEnvironment -> Flow s a) -> Flow s a)
withLet = def "withLet" $
  doc "Execute a computation with let context (adds let bindings to TypeContext)" $
  Schemas.withLetContext @@
    pythonEnvironmentGetTypeContext @@
    pythonEnvironmentSetTypeContext @@
    CoderUtils.bindingMetadata

-- | Execute a computation with inline let context (no metadata, for walrus operators)
--   Also adds binding names to inlineVariables so encodeVariable knows not to add call syntax.
withLetInline :: TBinding (PyHelpers.PythonEnvironment -> Let -> (PyHelpers.PythonEnvironment -> Flow s a) -> Flow s a)
withLetInline = def "withLetInline" $
  doc "Execute a computation with inline let context (for walrus operators)" $
  "env" ~> "lt" ~> "body" ~>
    "bindingNames" <~ (Lists.map ("b" ~> Core.bindingName (var "b")) (Core.letBindings $ var "lt")) $
    "inlineVars" <~ (Sets.fromList $ var "bindingNames") $
    "noMetadata" <~ ("tc" ~> "b" ~> (nothing :: TTerm (Maybe Term))) $
    Schemas.withLetContext @@
      pythonEnvironmentGetTypeContext @@
      pythonEnvironmentSetTypeContext @@
      var "noMetadata" @@
      var "env" @@
      var "lt" @@
      ("innerEnv" ~>
        "updatedEnv" <~ (record PyHelpers._PythonEnvironment [
          PyHelpers._PythonEnvironment_namespaces>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "innerEnv",
          PyHelpers._PythonEnvironment_boundTypeVariables>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "innerEnv",
          PyHelpers._PythonEnvironment_typeContext>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "innerEnv",
          PyHelpers._PythonEnvironment_nullaryBindings>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_nullaryBindings @@ var "innerEnv",
          PyHelpers._PythonEnvironment_version>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_version @@ var "innerEnv",
          PyHelpers._PythonEnvironment_skipCasts>>: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_skipCasts @@ var "innerEnv",
          PyHelpers._PythonEnvironment_inlineVariables>>: Sets.union (var "inlineVars")
            (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_inlineVariables @@ var "innerEnv")]) $
        var "body" @@ var "updatedEnv")

-- | Initial empty metadata for a Python module
initialMetadata :: TBinding (Namespace -> PyHelpers.PythonModuleMetadata)
initialMetadata = def "initialMetadata" $
  doc "Create initial empty metadata for a Python module" $
  "ns" ~>
    "dottedNs" <~ (PyNames.encodeNamespace @@ var "ns") $
    "emptyNs" <~ (Module.namespaces (pair (var "ns") (var "dottedNs")) Maps.empty) $
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>: var "emptyNs",
      PyHelpers._PythonModuleMetadata_typeVariables>>: Sets.empty,
      PyHelpers._PythonModuleMetadata_usesAnnotated>>: false,
      PyHelpers._PythonModuleMetadata_usesCallable>>: false,
      PyHelpers._PythonModuleMetadata_usesCast>>: false,
      PyHelpers._PythonModuleMetadata_usesLruCache>>: false,
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>: false,
      PyHelpers._PythonModuleMetadata_usesDataclass>>: false,
      PyHelpers._PythonModuleMetadata_usesDecimal>>: false,
      PyHelpers._PythonModuleMetadata_usesEither>>: false,
      PyHelpers._PythonModuleMetadata_usesEnum>>: false,
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>: false,
      PyHelpers._PythonModuleMetadata_usesFrozenList>>: false,
      PyHelpers._PythonModuleMetadata_usesGeneric>>: false,
      PyHelpers._PythonModuleMetadata_usesJust>>: false,
      PyHelpers._PythonModuleMetadata_usesLeft>>: false,
      PyHelpers._PythonModuleMetadata_usesMaybe>>: false,
      PyHelpers._PythonModuleMetadata_usesName>>: false,
      PyHelpers._PythonModuleMetadata_usesNode>>: false,
      PyHelpers._PythonModuleMetadata_usesNothing>>: false,
      PyHelpers._PythonModuleMetadata_usesRight>>: false,
      PyHelpers._PythonModuleMetadata_usesTypeVar>>: false]

-- | The target Python version for code generation (from Utils)
targetPythonVersion :: TBinding PyHelpers.PythonVersion
targetPythonVersion = def "targetPythonVersion" $
  doc "The target Python version for code generation" $
  PyUtils.targetPythonVersion

-- | Create an initial Python environment for code generation
initialEnvironment :: TBinding (Namespaces Py.DottedName -> TypeContext -> PyHelpers.PythonEnvironment)
initialEnvironment = def "initialEnvironment" $
  doc "Create an initial Python environment for code generation" $
  "namespaces" ~> "tcontext" ~>
    record PyHelpers._PythonEnvironment [
      PyHelpers._PythonEnvironment_namespaces>>: var "namespaces",
      PyHelpers._PythonEnvironment_boundTypeVariables>>: pair (list ([] :: [TTerm Name])) Maps.empty,
      PyHelpers._PythonEnvironment_typeContext>>: var "tcontext",
      PyHelpers._PythonEnvironment_nullaryBindings>>: Sets.empty,
      PyHelpers._PythonEnvironment_version>>: targetPythonVersion,
      PyHelpers._PythonEnvironment_skipCasts>>: true,
      PyHelpers._PythonEnvironment_inlineVariables>>: Sets.empty]

-- | Analyze a function term with Python-specific TypeContext management.
--   This is a wrapper around CoderUtils.analyzeFunctionTerm that provides the Python-specific
--   TypeContext getter and setter functions.
analyzePythonFunction :: TBinding (PyHelpers.PythonEnvironment -> Term -> Flow PyHelpers.PyGraph (FunctionStructure PyHelpers.PythonEnvironment))
analyzePythonFunction = def "analyzePythonFunction" $
  doc "Analyze a function term with Python-specific TypeContext management" $
  CoderUtils.analyzeFunctionTerm @@
    pythonEnvironmentGetTypeContext @@
    pythonEnvironmentSetTypeContext

-- | Like analyzePythonFunction but without recording binding metadata.
--   Used for inline lambda expressions where let bindings become walrus operators.
analyzePythonFunctionInline :: TBinding (PyHelpers.PythonEnvironment -> Term -> Flow PyHelpers.PyGraph (FunctionStructure PyHelpers.PythonEnvironment))
analyzePythonFunctionInline = def "analyzePythonFunctionInline" $
  doc "Analyze a function term without recording binding metadata (for inline lambdas)" $
  CoderUtils.analyzeFunctionTermInline @@
    pythonEnvironmentGetTypeContext @@
    pythonEnvironmentSetTypeContext

-- | Execute a computation with definitions in scope
withDefinitions :: TBinding (PyHelpers.PythonEnvironment -> [Definition] -> (PyHelpers.PythonEnvironment -> Flow s a) -> Flow s a)
withDefinitions = def "withDefinitions" $
  doc "Execute a computation with definitions in scope" $
  "env" ~> "defs" ~> "body" ~>
    -- Convert definitions to bindings for a dummy let
    "bindings" <~ (Maybes.cat $ Lists.map
      ("def_" ~>
        cases _Definition (var "def_") (Just nothing) [
          _Definition_term>>: "td" ~>
            just $ Core.binding
              (project _TermDefinition _TermDefinition_name @@ var "td")
              (project _TermDefinition _TermDefinition_term @@ var "td")
              (just $ project _TermDefinition _TermDefinition_type @@ var "td"),
          _Definition_type>>: constant nothing])
      (var "defs")) $
    "dummyLet" <~ (Core.let_ (var "bindings") (Core.termLiteral $ Core.literalString $ string "dummy")) $
    withLet @@ var "env" @@ var "dummyLet" @@ var "body"

-- | Encode a binding as a walrus operator assignment (for inline let expressions).
--   Takes: allowThunking flag, environment, binding
--   Returns: NamedExpression (assignment expression)
--   Note: This simplified version does not update metadata for lru_cache;
--   the Staging version handles that.
encodeBindingAsAssignment :: TBinding (Bool -> PyHelpers.PythonEnvironment
  -> Binding
  -> Flow PyHelpers.PyGraph Py.NamedExpression)
encodeBindingAsAssignment = def "encodeBindingAsAssignment" $
  doc "Encode a binding as a walrus operator assignment" $
  "allowThunking" ~> "env" ~> "binding" ~>
    "name" <~ Core.bindingName (var "binding") $
    "term" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    "pyName" <~ (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name") $
    "pbody" <<~ (encodeTermInline @@ var "env" @@ false @@ var "term") $
    "tc" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env") $
    "isComplexVar" <~ (CoderUtils.isComplexVariable @@ var "tc" @@ var "name") $
    "termIsComplex" <~ (CoderUtils.isComplexTerm @@ var "tc" @@ var "term") $
    -- Check if needs thunking based on type scheme arity and complexity
    "needsThunk" <~ optCases (var "mts")
      -- No type scheme: thunk if complex
      (Logic.and (var "allowThunking") (Logic.or (var "isComplexVar") (var "termIsComplex")))
      -- Has type scheme: thunk if arity == 0 and complex
      ("ts" ~> Logic.and (var "allowThunking")
        (Logic.and (Equality.equal (Arity.typeSchemeArity @@ var "ts") (Phantoms.int 0))
                   (Logic.or (var "isComplexVar") (var "termIsComplex")))) $
    "pterm" <~ (Logic.ifElse (var "needsThunk") (makeThunk @@ var "pbody") (var "pbody")) $
    produce $ PyDsl.namedExpressionAssignment $ PyDsl.assignmentExpression (var "pyName") (var "pterm")

-- | Encode a function definition with parameters and body.
--   Takes: environment, name, type params, arg names, body term, domain types, optional codomain, comment, prefix statements
encodeFunctionDefinition :: TBinding (PyHelpers.PythonEnvironment
  -> Name -> [Name] -> [Name] -> TTerm Term -> [Type] -> Maybe Type -> Maybe String -> [Py.Statement]
  -> Flow PyHelpers.PyGraph Py.Statement)
encodeFunctionDefinition = def "encodeFunctionDefinition" $
  doc "Encode a function definition with parameters and body" $
  "env" ~> "name" ~> "tparams" ~> "args" ~> "body" ~> "doms" ~> "mcod" ~> "comment" ~> "prefixes" ~>
    -- Create parameters by zipping arg names with domain types
    "pyArgs" <<~ Flows.mapList
      ("pair" ~>
        "argName" <~ Pairs.first (var "pair") $
        "typ" <~ Pairs.second (var "pair") $
        "pyTyp" <<~ (encodeType @@ var "env" @@ var "typ") $
        produce $ PyDsl.paramNoDefaultSimple $ PyDsl.param
          (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "argName")
          (just $ PyDsl.annotation $ var "pyTyp"))
      (Lists.zip (var "args") (var "doms")) $
    "pyParams" <~ (PyDsl.parametersParamNoDefault $ PyDsl.paramNoDefaultParameters (var "pyArgs") (Phantoms.list ([] :: [TTerm Py.ParamWithDefault])) nothing) $
    "stmts" <<~ (encodeTermMultiline @@ var "env" @@ var "body") $
    "block" <~ (PyUtils.indentedBlock @@ var "comment" @@ list [Lists.concat2 (var "prefixes") (var "stmts")]) $
    -- Encode return type if present
    "mreturnType" <<~ optCases (var "mcod")
      (Flows.pure (nothing :: TTerm (Maybe Py.Expression)))
      ("cod" ~>
        "pytyp" <<~ (encodeType @@ var "env" @@ var "cod") $
        produce $ just (var "pytyp")) $
    -- Type parameters (only for Python 3.12+)
    "pyTparams" <~ (Logic.ifElse useInlineTypeParams
      (Lists.map (PyUtils.pyNameToPyTypeParameter <.> PyNames.encodeTypeVariable) (var "tparams"))
      (Phantoms.list ([] :: [TTerm Py.TypeParameter]))) $
    -- Check if this is a thunk (zero-argument function)
    "isThunk" <~ (Lists.null $ var "args") $
    "mDecorators" <~ (Logic.ifElse (var "isThunk")
      (just $ wrap Py._Decorators $ list [lruCacheDecorator])
      nothing) $
    -- Update metadata to indicate lru_cache is used when this is a thunk
    "unit1" <<~ (Logic.ifElse (var "isThunk")
      (updateMeta @@ (setMetaUsesLruCache @@ true))
      (Flows.pure unit)) $
    "pyName" <~ (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name") $
    produce $ PyDsl.statementCompound $ PyDsl.compoundStatementFunction $ PyDsl.functionDefinition (var "mDecorators") $
      PyDsl.functionDefRaw false (var "pyName") (var "pyTparams") (just $ var "pyParams") (var "mreturnType") nothing (var "block")

-- | Encode a term to a list of statements, with the last statement as the return value.
--   This handles case statements specially by generating match statements.
encodeTermMultiline :: TBinding (PyHelpers.PythonEnvironment
  -> TTerm Term
  -> Flow PyHelpers.PyGraph [Py.Statement])
encodeTermMultiline = def "encodeTermMultiline" $
  doc "Encode a term to a list of statements with return as final statement" $
  "env" ~> "term" ~>
    -- Define the default/fallback logic that handles non-case-statement terms
    "dfltLogic" <~
      ("fs" <<~ (analyzePythonFunction @@ var "env" @@ var "term") $
        "params" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_params @@ var "fs") $
        "bindings" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_bindings @@ var "fs") $
        "innerBody" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_body @@ var "fs") $
        "env2" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_environment @@ var "fs") $
        Logic.ifElse (Logic.not $ Lists.null (var "params"))
          (Flows.fail $ string "Functions currently unsupported in this context")
          (Logic.ifElse (Lists.null $ var "bindings")
            -- No bindings: encode inline and wrap in return
            ("expr" <<~ (encodeTermInline @@ var "env" @@ false @@ var "term") $
              produce $ list [PyUtils.returnSingle @@ var "expr"])
            -- Has bindings: encode bindings as defs, then recurse on body
            (withBindings @@ var "bindings" @@
              ("bindingStmts" <<~ (Flows.mapList (encodeBindingAs @@ var "env2") (var "bindings")) $
                "bodyStmts" <<~ (encodeTermMultiline @@ var "env2" @@ var "innerBody") $
                produce $ Lists.concat2 (var "bindingStmts") (var "bodyStmts"))))) $
    "gathered" <~ (CoderUtils.gatherApplications @@ var "term") $
    "args" <~ Pairs.first (var "gathered") $
    "body" <~ Pairs.second (var "gathered") $
    -- Check if exactly one argument for potential case statement
    Logic.ifElse (Equality.equal (Lists.length $ var "args") (int32 1))
      -- Try to handle case statement specially
      ("arg" <~ (Lists.head $ var "args") $
        cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "body") (Just $ var "dfltLogic") [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just $ var "dfltLogic") [
              _Function_elimination>>: "e" ~>
                cases _Elimination (var "e") (Just $ var "dfltLogic") [
                  _Elimination_union>>: "cs" ~>
                    "tname" <~ (Core.caseStatementTypeName $ var "cs") $
                    "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                    "cases_" <~ (Core.caseStatementCases $ var "cs") $
                    "rt" <<~ (inGraphContext @@ (Schemas.requireUnionType @@ var "tname")) $
                    "isEnum" <~ (Schemas.isEnumRowType @@ var "rt") $
                    "isFull" <~ (isCasesFull @@ var "rt" @@ var "cases_") $
                    "pyArg" <<~ (encodeTermInline @@ var "env" @@ false @@ var "arg") $
                    "pyCases" <<~ (Flows.mapList (encodeCaseBlock @@ var "env" @@ var "tname" @@ var "rt" @@ var "isEnum" @@ ("e" ~> "t" ~> encodeTermMultiline @@ var "e" @@ var "t")) (deduplicateCaseVariables @@ var "cases_")) $
                    "pyDflt" <<~ (encodeDefaultCaseBlock @@ ("t" ~> encodeTermInline @@ var "env" @@ false @@ var "t") @@ var "isFull" @@ var "dflt" @@ var "tname") $
                    "subj" <~ (PyDsl.subjectExpressionSimple $ PyDsl.namedExpressionSimple $ var "pyArg") $
                    "matchStmt" <~ (PyDsl.statementCompound $ PyDsl.compoundStatementMatch $ Phantoms.record Py._MatchStatement [
                      Phantoms.field Py._MatchStatement_subject (var "subj"),
                      Phantoms.field Py._MatchStatement_cases (Lists.concat2 (var "pyCases") (var "pyDflt"))]) $
                    produce $ list [var "matchStmt"]]]])
      -- Default case: use the fallback logic
      (var "dfltLogic")

-- | Encode a function term to a Python expression.
--   This handles lambdas, primitives, projections, wrap eliminations, and case eliminations.
encodeFunction :: TBinding (PyHelpers.PythonEnvironment
  -> Function
  -> Flow PyHelpers.PyGraph Py.Expression)
encodeFunction = def "encodeFunction" $
  doc "Encode a function term to a Python expression" $
  "env" ~> "f" ~>
    cases _Function (var "f") Nothing [
      _Function_lambda>>: "lam" ~>
        -- Use analyzePythonFunctionInline for inline lambda expressions
        "fs" <<~ (analyzePythonFunctionInline @@ var "env" @@ (Core.termFunction $ Core.functionLambda $ var "lam")) $
        "params" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_params @@ var "fs") $
        "bindings" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_bindings @@ var "fs") $
        "innerBody" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_body @@ var "fs") $
        "innerEnv" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_environment @@ var "fs") $
        "pbody" <<~ (encodeTermInline @@ var "innerEnv" @@ false @@ var "innerBody") $
        "pparams" <~ (Lists.map (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "innerEnv") (var "params")) $
        Logic.ifElse (Lists.null $ var "bindings")
          (produce $ makeUncurriedLambda @@ var "pparams" @@ var "pbody")
          -- Has bindings: create walrus operator expressions
          ("pbindingExprs" <<~ (Flows.mapList (encodeBindingAsAssignment @@ false @@ var "innerEnv") (var "bindings")) $
            "pbindingStarExprs" <~ (Lists.map ("ne" ~> PyDsl.starNamedExpressionSimple (var "ne")) (var "pbindingExprs")) $
            "pbodyStarExpr" <~ (PyUtils.pyExpressionToPyStarNamedExpression @@ var "pbody") $
            "tupleElements" <~ (Lists.concat2 (var "pbindingStarExprs") (list [var "pbodyStarExpr"])) $
            "tupleExpr" <~ (PyUtils.pyAtomToPyExpression @@ (PyDsl.atomTuple $ PyDsl.tuple $ var "tupleElements")) $
            "indexValue" <~ (PyUtils.pyAtomToPyExpression @@ (PyDsl.atomNumber $ PyDsl.numberInteger $ Literals.int32ToBigint (Lists.length (var "bindings")))) $
            "indexedExpr" <~ (PyUtils.primaryWithExpressionSlices @@ (PyUtils.pyExpressionToPyPrimary @@ var "tupleExpr") @@ list [var "indexValue"]) $
            produce $ makeUncurriedLambda @@ var "pparams" @@ (PyUtils.pyPrimaryToPyExpression @@ var "indexedExpr")),
      -- Primitives: encode as variable reference
      _Function_primitive>>: "name" ~>
        encodeVariable @@ var "env" @@ var "name" @@ (Phantoms.list ([] :: [TTerm Py.Expression])),
      -- Eliminations
      _Function_elimination>>: "e" ~>
        cases _Elimination (var "e") Nothing [
          -- Record projection: lambda v1: v1.field
          _Elimination_record>>: "proj" ~>
            "fname" <~ (Phantoms.project _Projection _Projection_field @@ var "proj") $
            produce $ makeCurriedLambda @@ list [PyDsl.name $ string "v1"] @@
              (PyUtils.projectFromExpression @@ (PyDsl.pyNameToPyExpression $ PyDsl.name $ string "v1") @@ (PyNames.encodeFieldName @@ var "env" @@ var "fname")),
          -- Wrap elimination: lambda v1: v1.value
          _Elimination_wrap>>: constant $
            produce $ makeCurriedLambda @@ list [PyDsl.name $ string "v1"] @@
              (PyUtils.projectFromExpression @@ (PyDsl.pyNameToPyExpression $ PyDsl.name $ string "v1") @@ (PyDsl.name $ string "value")),
          -- Union elimination (case) as value: not supported in Python
          _Elimination_union>>: constant $
            produce $ unsupportedExpression @@ string "case expressions as values are not yet supported"]]

-- | Encode a term assignment (top-level binding) to a Python statement.
--   This dispatches to either a simple assignment or a function definition depending on complexity.
encodeTermAssignment :: TBinding (PyHelpers.PythonEnvironment
  -> Name -> TTerm Term -> TypeScheme -> Maybe String
  -> Flow PyHelpers.PyGraph Py.Statement)
encodeTermAssignment = def "encodeTermAssignment" $
  doc "Encode a term assignment to a Python statement" $
  "env" ~> "name" ~> "term" ~> "ts" ~> "comment" ~>
    "fs" <<~ (analyzePythonFunction @@ var "env" @@ var "term") $
    "tparams" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_typeParams @@ var "fs") $
    "params" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_params @@ var "fs") $
    "bindings" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_bindings @@ var "fs") $
    "body" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_body @@ var "fs") $
    "doms" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_domains @@ var "fs") $
    "mcod" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_codomain @@ var "fs") $
    "env2" <~ (Phantoms.project HydraTyping._FunctionStructure HydraTyping._FunctionStructure_environment @@ var "fs") $
    "tc" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env2") $
    "binding" <~ (Core.binding (var "name") (var "term") (just $ var "ts")) $
    "isComplex" <~ (CoderUtils.isComplexBinding @@ var "tc" @@ var "binding") $
    Logic.ifElse (var "isComplex")
      -- Complex binding: use function definition
      (withBindings @@ var "bindings" @@
        ("bindingStmts" <<~ (Flows.mapList (encodeBindingAs @@ var "env2") (var "bindings")) $
          encodeFunctionDefinition @@ var "env2" @@ var "name" @@ var "tparams" @@ var "params" @@ var "body" @@ var "doms" @@ var "mcod" @@ var "comment" @@ var "bindingStmts"))
      -- Simple binding: use assignment
      ("bodyExpr" <<~ (encodeTermInline @@ var "env2" @@ false @@ var "body") $
        "pyName" <~ (PyNames.encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env2" @@ var "name") $
        produce $ PyUtils.annotatedStatement @@ var "comment" @@ (PyUtils.assignmentStatement @@ var "pyName" @@ var "bodyExpr"))

-- | Encode a variable reference to a Python expression.
--   This handles various cases: lambda variables, let-bound variables, primitives, and graph elements.
--   The complexity arises from needing to determine when a variable needs call syntax () vs plain reference.
encodeVariable :: TBinding (PyHelpers.PythonEnvironment -> Name -> [Py.Expression] -> Flow PyHelpers.PyGraph Py.Expression)
encodeVariable = def "encodeVariable" $
  doc "Encode a variable reference to a Python expression" $
  "env" ~> "name" ~> "args" ~>
    "pyg" <<~ Monads.getState $
    "g" <~ (pyGraphGraph @@ var "pyg") $
    "tc" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env") $
    "tcTypes" <~ (Typing.typeContextTypes $ var "tc") $
    "tcLambdaVars" <~ (Typing.typeContextLambdaVariables $ var "tc") $
    "tcMetadata" <~ (Typing.typeContextMetadata $ var "tc") $
    "inlineVars" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_inlineVariables @@ var "env") $
    "mTyp" <~ (Maps.lookup (var "name") (var "tcTypes")) $
    "asVariable" <~ (PyNames.termVariableReference @@ var "env" @@ var "name") $
    "asFunctionCall" <~ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeName @@ true @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name")) @@ var "args") $
    Logic.ifElse (Logic.not $ Lists.null (var "args"))
      -- Non-empty args: check for primitives first
      (Maybes.maybe
        -- No primitive found: use regular function call
        (produce $ var "asFunctionCall")
        -- Primitive found: check if full or partial application
        ("prim" ~>
          "primArity" <~ (Arity.primitiveArity @@ var "prim") $
          Logic.ifElse (Equality.equal (var "primArity") (Lists.length (var "args")))
            -- Full application
            (produce $ var "asFunctionCall")
            -- Partial application: create lambda for remaining args
            ("numRemaining" <~ (Math.sub (var "primArity") (Lists.length (var "args"))) $
              "remainingParams" <~ (Lists.map ("i" ~> PyDsl.name (Strings.cat2 (string "x") (Literals.showInt32 (var "i")))) (Math.range (int32 1) (var "numRemaining"))) $
              "remainingExprs" <~ (Lists.map ("n" ~> PyDsl.pyNameToPyExpression (var "n")) (var "remainingParams")) $
              "allArgs" <~ (Lists.concat2 (var "args") (var "remainingExprs")) $
              "fullCall" <~ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeName @@ true @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name")) @@ var "allArgs") $
              produce $ makeUncurriedLambda @@ var "remainingParams" @@ var "fullCall"))
        (Lexical.lookupPrimitive @@ var "g" @@ var "name"))
      -- Empty args: check various contexts
      (Maybes.maybe
        -- Name not in typeContextTypes
        (Logic.ifElse (Sets.member (var "name") (var "tcLambdaVars"))
          -- Untyped lambda variable
          (produce $ var "asVariable")
          -- Not a lambda var - check primitives
          (Maybes.maybe
            -- Not a primitive - check graph elements
            (Maybes.maybe
              -- Not in graph elements - check metadata
              (Maybes.maybe
                (Flows.fail $ Strings.cat2 (string "Unknown variable: ") (Core.unName (var "name")))
                (constant $ produce $ var "asFunctionCall")  -- Lifted case expression
                (Maps.lookup (var "name") (var "tcMetadata")))
              -- In graph elements
              ("el" ~>
                Maybes.maybe
                  (produce $ var "asVariable")
                  ("ts" ~>
                    Logic.ifElse (Logic.and (Equality.equal (Arity.typeSchemeArity @@ var "ts") (int32 0))
                                            (CoderUtils.isComplexBinding @@ var "tc" @@ var "el"))
                      (produce $ var "asFunctionCall")
                      ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Lists.null (Core.typeSchemeVariables $ var "ts"))
                          (makeSimpleLambda @@ (Arity.typeArity @@ (Core.typeSchemeType $ var "ts")) @@ var "asVariable")
                          (var "asVariable")) $
                        produce $ var "asFunctionRef"))
                  (Core.bindingType $ var "el"))
              (Lexical.lookupElement @@ var "g" @@ var "name"))
            -- Is a primitive with no args: check if nullary
            ("prim" ~>
              "primArity" <~ (Arity.primitiveArity @@ var "prim") $
              Logic.ifElse (Equality.equal (var "primArity") (int32 0))
                -- Nullary primitive: call with ()
                (produce $ var "asFunctionCall")
                -- Non-nullary primitive: function reference
                ("ts" <~ (Phantoms.project _Primitive _Primitive_type @@ var "prim") $
                  "asFunctionRef" <~ (Logic.ifElse (Logic.not $ Lists.null (Core.typeSchemeVariables $ var "ts"))
                      (makeSimpleLambda @@ (Arity.typeArity @@ (Core.typeSchemeType $ var "ts")) @@ var "asVariable")
                      (var "asVariable")) $
                  produce $ var "asFunctionRef"))
            (Lexical.lookupPrimitive @@ var "g" @@ var "name")))
        -- Name is in typeContextTypes
        ("typ" ~>
          Logic.ifElse (Sets.member (var "name") (var "tcLambdaVars"))
            -- Lambda variable
            (produce $ var "asVariable")
            -- Not a lambda variable
            (Logic.ifElse (Sets.member (var "name") (var "inlineVars"))
              -- Inline variable: function reference
              ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
                  (makeSimpleLambda @@ (Arity.typeArity @@ var "typ") @@ var "asVariable")
                  (var "asVariable")) $
                produce $ var "asFunctionRef")
              -- Not inline variable
              (Logic.ifElse (Logic.not $ Maps.member (var "name") (var "tcMetadata"))
                -- Not in metadata - check graph elements
                (Maybes.maybe
                  -- Not in graph elements: inline let binding
                  ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
                      (makeSimpleLambda @@ (Arity.typeArity @@ var "typ") @@ var "asVariable")
                      (var "asVariable")) $
                    produce $ var "asFunctionRef")
                  -- In graph elements
                  ("el" ~>
                    Maybes.maybe
                      (Logic.ifElse (Equality.equal (Arity.typeArity @@ var "typ") (int32 0))
                        (produce $ var "asFunctionCall")
                        ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
                            (makeSimpleLambda @@ (Arity.typeArity @@ var "typ") @@ var "asVariable")
                            (var "asVariable")) $
                          produce $ var "asFunctionRef"))
                      ("ts" ~>
                        Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity @@ var "typ") (int32 0))
                                                (CoderUtils.isComplexBinding @@ var "tc" @@ var "el"))
                          (produce $ var "asFunctionCall")
                          ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
                              (makeSimpleLambda @@ (Arity.typeArity @@ var "typ") @@ var "asVariable")
                              (var "asVariable")) $
                            produce $ var "asFunctionRef"))
                      (Core.bindingType $ var "el"))
                  (Lexical.lookupElement @@ var "g" @@ var "name"))
                -- Is in metadata: regular let binding
                (Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity @@ var "typ") (int32 0))
                                          (CoderUtils.isComplexVariable @@ var "tc" @@ var "name"))
                  (produce $ var "asFunctionCall")
                  ("asFunctionRef" <~ (Logic.ifElse (Logic.not $ Sets.null (Rewriting.freeVariablesInType @@ var "typ"))
                      (makeSimpleLambda @@ (Arity.typeArity @@ var "typ") @@ var "asVariable")
                      (var "asVariable")) $
                    produce $ var "asFunctionRef")))))
        (var "mTyp"))

-- | Encode a function application to a Python expression.
--   This is a complex function that handles various application patterns:
--   - Record projection (field access)
--   - Union case elimination (match expressions - currently unsupported inline)
--   - Wrap elimination (unwrapping newtypes)
--   - Primitive applications
--   - Variable applications
encodeApplication :: TBinding (PyHelpers.PythonEnvironment
  -> Application
  -> Flow PyHelpers.PyGraph Py.Expression)
encodeApplication = def "encodeApplication" $
  doc "Encode a function application to a Python expression" $
  "env" ~> "app" ~>
    "pyg" <<~ Monads.getState $
    "g" <~ (pyGraphGraph @@ var "pyg") $
    "tc" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env") $
    "skipCasts" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_skipCasts @@ var "env") $
    "term" <~ (Core.termApplication $ var "app") $
    "gathered" <~ (CoderUtils.gatherArgs @@ var "term" @@ list ([] :: [TTerm Term])) $
    "fun" <~ (Pairs.first $ var "gathered") $
    "args" <~ (Pairs.second $ var "gathered") $
    -- Try to get arity from type; fall back to term-based arity
    "termArity" <~ (termArityWithPrimitives @@ var "g" @@ var "fun") $
    "arity" <<~ (Flows.withDefault (var "termArity") $
      Flows.map Arity.typeArity (inGraphContext @@ (Checking.typeOf @@ var "tc" @@ (Phantoms.list ([] :: [TTerm Type])) @@ var "fun"))) $
    "pargs" <<~ (Flows.mapList ("t" ~> encodeTermInline @@ var "env" @@ false @@ var "t") (var "args")) $
    "hargs" <~ (Lists.take (var "arity") (var "pargs")) $
    "rargs" <~ (Lists.drop (var "arity") (var "pargs")) $
    -- Apply args based on function type
    "result" <<~ (encodeApplicationInner @@ var "env" @@ var "fun" @@ var "hargs" @@ var "rargs") $
    "lhs" <~ (Pairs.first $ var "result") $
    "remainingRargs" <~ (Pairs.second $ var "result") $
    -- Fold remaining args into function calls
    "pyapp" <~ (Lists.foldl
      ("t" ~> "a" ~> PyUtils.functionCall @@ (PyUtils.pyExpressionToPyPrimary @@ var "t") @@ (list [var "a"]))
      (var "lhs")
      (var "remainingRargs")) $
    produce $ var "pyapp"

-- | Inner helper for encodeApplication that handles the different function types.
--   Returns (expression, remaining rargs).
encodeApplicationInner :: TBinding (PyHelpers.PythonEnvironment
  -> TTerm Term  -- fun
  -> [Py.Expression]  -- hargs
  -> [Py.Expression]  -- rargs
  -> Flow PyHelpers.PyGraph (Py.Expression, [Py.Expression]))
encodeApplicationInner = def "encodeApplicationInner" $
  doc "Inner helper for encodeApplication" $
  "env" ~> "fun" ~> "hargs" ~> "rargs" ~>
    "firstArg" <~ (Lists.head $ var "hargs") $
    "restArgs" <~ (Lists.tail $ var "hargs") $
    "withRest" <~ ("e" ~>
      Logic.ifElse (Lists.null $ var "restArgs")
        (var "e")
        (PyUtils.functionCall @@ (PyUtils.pyExpressionToPyPrimary @@ var "e") @@ var "restArgs")) $
    -- Default case: encode function and apply
    "defaultCase" <~ ("pfun" <<~ (encodeTermInline @@ var "env" @@ false @@ var "fun") $
      produce $ pair (PyUtils.functionCall @@ (PyUtils.pyExpressionToPyPrimary @@ var "pfun") @@ var "hargs") (var "rargs")) $
    cases _Term (Rewriting.deannotateTerm @@ var "fun") (Just $ var "defaultCase") [
      _Term_function>>: "f" ~>
        cases _Function (var "f") (Just $ var "defaultCase") [
          _Function_elimination>>: "elm" ~>
            cases _Elimination (var "elm") (Just $ var "defaultCase") [
              -- Record projection: obj.field
              _Elimination_record>>: "proj" ~>
                "fname" <~ (project _Projection _Projection_field @@ var "proj") $
                "fieldExpr" <~ (PyUtils.projectFromExpression @@ var "firstArg" @@ (PyNames.encodeFieldName @@ var "env" @@ var "fname")) $
                produce $ pair (var "withRest" @@ var "fieldExpr") (var "rargs"),
              -- Union elimination: not supported inline
              _Elimination_union>>: constant $
                produce $ pair (unsupportedExpression @@ string "inline match expressions are not yet supported") (var "rargs"),
              -- Wrap elimination: obj.value
              _Elimination_wrap>>: constant $
                "valueExpr" <~ (PyUtils.projectFromExpression @@ var "firstArg" @@ (PyDsl.name $ string "value")) $
                "allArgs" <~ (Lists.concat2 (var "restArgs") (var "rargs")) $
                Logic.ifElse (Lists.null $ var "allArgs")
                  (produce $ pair (var "valueExpr") (list ([] :: [TTerm Py.Expression])))
                  (produce $ pair (PyUtils.functionCall @@ (PyUtils.pyExpressionToPyPrimary @@ var "valueExpr") @@ var "allArgs")
                                  (list ([] :: [TTerm Py.Expression])))],
          -- Primitive: encode variable with args (wrap lazy arguments for primitives like ifElse)
          _Function_primitive>>: "name" ~>
            "wrappedArgs" <~ (wrapLazyArguments @@ var "name" @@ var "hargs") $
            "expr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ var "wrappedArgs") $
            produce $ pair (var "expr") (var "rargs"),
          -- Other functions: encode and apply
          _Function_lambda>>: constant $
            "pfun" <<~ (encodeTermInline @@ var "env" @@ false @@ var "fun") $
            produce $ pair (PyUtils.functionCall @@ (PyUtils.pyExpressionToPyPrimary @@ var "pfun") @@ var "hargs") (var "rargs")],
      -- Variable: encode and apply
      _Term_variable>>: "name" ~>
        "pyg" <<~ Monads.getState $
        "g" <~ (pyGraphGraph @@ var "pyg") $
        "allArgs" <~ (Lists.concat2 (var "hargs") (var "rargs")) $
        Maybes.maybe
          -- Not in graph elements: use encodeVariable
          ("expr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ var "hargs") $
            produce $ pair (var "expr") (var "rargs"))
          -- In graph elements: check arity
          ("el" ~>
            Maybes.maybe
              -- No type: use encodeVariable
              ("expr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ var "hargs") $
                produce $ pair (var "expr") (var "rargs"))
              -- Has type: use arity
              ("ts" ~>
                "elArity" <~ (Arity.typeSchemeArity @@ var "ts") $
                "consumeCount" <~ (Math.min (var "elArity") (Lists.length $ var "allArgs")) $
                "consumedArgs" <~ (Lists.take (var "consumeCount") (var "allArgs")) $
                "remainingArgs" <~ (Lists.drop (var "consumeCount") (var "allArgs")) $
                Logic.ifElse (Lists.null $ var "consumedArgs")
                  ("expr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ (list ([] :: [TTerm Py.Expression]))) $
                    produce $ pair (var "expr") (var "rargs"))
                  (produce $ pair
                    (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeName @@ true @@ Util.caseConventionLowerSnake @@ var "env" @@ var "name")) @@ var "consumedArgs")
                    (var "remainingArgs")))
              (Core.bindingType $ var "el"))
          (Lexical.lookupElement @@ var "g" @@ var "name")]

-- | Encode a term to a Python expression (inline form).
--   This is the main term encoding function that handles all term variants.
--   Parameters: environment, noCast flag, term
encodeTermInline :: TBinding (PyHelpers.PythonEnvironment
  -> Bool
  -> TTerm Term
  -> Flow PyHelpers.PyGraph Py.Expression)
encodeTermInline = def "encodeTermInline" $
  doc "Encode a term to a Python expression (inline form)" $
  "env" ~> "noCast" ~> "term" ~>
    -- Helper for recursive encoding (self-reference)
    "encode" <~ ("t" ~> encodeTermInline @@ var "env" @@ false @@ var "t") $
    -- Helper to strip type applications and annotations
    "stripTypeApps" <~
      ("t" ~> cases _Term (var "t") (Just $ var "t") [
        _Term_annotated>>: "ann" ~>
          var "stripTypeApps" @@ Core.annotatedTermBody (var "ann"),
        _Term_typeApplication>>: "ta" ~>
          var "stripTypeApps" @@ Core.typeApplicationTermBody (var "ta")]) $
    -- withCast helper: adds cast() around expression when type is available
    -- If noCast is true or skipCasts is enabled, just return the expression
    -- Otherwise try to infer the type and wrap in cast(type, expr)
    "withCast" <~ ("pyexp" ~>
      -- Check if we should skip casting
      Logic.ifElse (Logic.or (var "noCast")
                             (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_skipCasts @@ var "env"))
        -- Skip casting: just return the expression
        (Flows.pure $ var "pyexp")
        -- Try to get the type and wrap in cast if successful
        ("tc" <~ (project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_typeContext @@ var "env") $
         -- Use withDefault to handle type inference failures gracefully
         Flows.withDefault (var "pyexp")
           ("typ" <<~ (Checking.typeOf @@ var "tc" @@ list ([] :: [TTerm Type]) @@ var "term") $
            "pytyp" <<~ (encodeType @@ var "env" @@ var "typ") $
            -- Update metadata to indicate cast is used - inline the record update
            "unit_" <<~ (updateMeta @@ ("m" ~>
              record PyHelpers._PythonModuleMetadata [
                PyHelpers._PythonModuleMetadata_namespaces>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
                PyHelpers._PythonModuleMetadata_typeVariables>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
                PyHelpers._PythonModuleMetadata_usesAnnotated>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
                PyHelpers._PythonModuleMetadata_usesCallable>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
                PyHelpers._PythonModuleMetadata_usesCast>>: true,
                PyHelpers._PythonModuleMetadata_usesLruCache>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
                PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
                PyHelpers._PythonModuleMetadata_usesDataclass>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
                PyHelpers._PythonModuleMetadata_usesDecimal>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
                PyHelpers._PythonModuleMetadata_usesEither>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
                PyHelpers._PythonModuleMetadata_usesEnum>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
                PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
                PyHelpers._PythonModuleMetadata_usesFrozenList>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
                PyHelpers._PythonModuleMetadata_usesGeneric>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
                PyHelpers._PythonModuleMetadata_usesJust>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
                PyHelpers._PythonModuleMetadata_usesLeft>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
                PyHelpers._PythonModuleMetadata_usesMaybe>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
                PyHelpers._PythonModuleMetadata_usesName>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
                PyHelpers._PythonModuleMetadata_usesNode>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
                PyHelpers._PythonModuleMetadata_usesNothing>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
                PyHelpers._PythonModuleMetadata_usesRight>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
                PyHelpers._PythonModuleMetadata_usesTypeVar>>:
                  project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"])) $
            produce $ PyUtils.castTo @@ var "pytyp" @@ var "pyexp"))) $
    -- Main case dispatch on term variant
    cases _Term (Rewriting.deannotateTerm @@ var "term") Nothing [
      -- TermApplication
      _Term_application>>: "app" ~>
        encodeApplication @@ var "env" @@ var "app",

      -- TermEither
      _Term_either>>: "et" ~>
        Eithers.either_
          ("t1" ~>
            "pyexp" <<~ (var "encode" @@ var "t1") $
            var "withCast" @@ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "Left")) @@ list [var "pyexp"]))
          ("t1" ~>
            "pyexp" <<~ (var "encode" @@ var "t1") $
            var "withCast" @@ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "Right")) @@ list [var "pyexp"]))
          (var "et"),

      -- TermFunction
      _Term_function>>: "f" ~>
        encodeFunction @@ var "env" @@ var "f",

      -- TermLet - encode using walrus operators in a tuple
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        Logic.ifElse (Lists.null $ var "bindings")
          (encodeTermInline @@ var "env" @@ false @@ var "body")
          (withLetInline @@ var "env" @@ var "lt" @@
            ("innerEnv" ~>
              "pbindingExprs" <<~ (Flows.mapList (encodeBindingAsAssignment @@ false @@ var "innerEnv") (var "bindings")) $
              "pbody" <<~ (encodeTermInline @@ var "innerEnv" @@ false @@ var "body") $
              "pbindingStarExprs" <~ (Lists.map ("ne" ~> PyDsl.starNamedExpressionSimple (var "ne")) (var "pbindingExprs")) $
              "pbodyStarExpr" <~ (PyUtils.pyExpressionToPyStarNamedExpression @@ var "pbody") $
              "tupleElements" <~ (Lists.concat2 (var "pbindingStarExprs") (list [var "pbodyStarExpr"])) $
              "tupleExpr" <~ (PyUtils.pyAtomToPyExpression @@ PyDsl.atomTuple (PyDsl.tuple (var "tupleElements"))) $
              "indexValue" <~ (PyUtils.pyAtomToPyExpression @@ PyDsl.atomNumber (PyDsl.numberInteger (Literals.int32ToBigint (Lists.length $ var "bindings")))) $
              "indexedExpr" <~ (PyUtils.primaryWithExpressionSlices @@ (PyUtils.pyExpressionToPyPrimary @@ var "tupleExpr") @@ list [var "indexValue"]) $
              produce $ PyUtils.pyPrimaryToPyExpression @@ var "indexedExpr")),

      -- TermList - encode as tuple
      _Term_list>>: "terms" ~>
        "pyExprs" <<~ (Flows.mapList (var "encode") (var "terms")) $
        produce $ PyUtils.pyAtomToPyExpression @@ PyDsl.atomTuple (PyDsl.tuple (Lists.map PyUtils.pyExpressionToPyStarNamedExpression (var "pyExprs"))),

      -- TermLiteral
      _Term_literal>>: "lit" ~>
        encodeLiteral @@ var "lit",

      -- TermMap - encode as FrozenDict
      _Term_map>>: "m" ~>
        "pairs" <<~ (Flows.mapList
          ("kv" ~>
            "k" <~ (Pairs.first $ var "kv") $
            "v" <~ (Pairs.second $ var "kv") $
            "pyK" <<~ (var "encode" @@ var "k") $
            "pyV" <<~ (var "encode" @@ var "v") $
            produce $ PyDsl.doubleStarredKvpairPair (PyDsl.kvpair (var "pyK") (var "pyV")))
          (Maps.toList $ var "m")) $
        produce $ PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "FrozenDict"))
          @@ list [PyUtils.pyAtomToPyExpression @@ PyDsl.atomDict (PyDsl.dict (var "pairs"))],

      -- TermMaybe - encode as Nothing() or Just(value)
      _Term_maybe>>: "mt" ~>
        Maybes.maybe
          (produce $ PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "Nothing")) @@ list ([] :: [TTerm Py.Expression]))
          ("t1" ~>
            "pyexp" <<~ (var "encode" @@ var "t1") $
            var "withCast" @@ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "Just")) @@ list [var "pyexp"]))
          (var "mt"),

      -- TermPair - encode as 2-tuple
      _Term_pair>>: "p" ~>
        "t1" <~ (Pairs.first $ var "p") $
        "t2" <~ (Pairs.second $ var "p") $
        "pyExpr1" <<~ (var "encode" @@ var "t1") $
        "pyExpr2" <<~ (var "encode" @@ var "t2") $
        produce $ PyUtils.pyAtomToPyExpression @@ PyDsl.atomTuple (PyDsl.tuple
          (list [PyUtils.pyExpressionToPyStarNamedExpression @@ var "pyExpr1", PyUtils.pyExpressionToPyStarNamedExpression @@ var "pyExpr2"])),

      -- TermRecord
      _Term_record>>: "r" ~>
        "tname" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        "pargs" <<~ (Flows.mapList ("fld" ~> var "encode" @@ Core.fieldTerm (var "fld")) (var "fields")) $
        produce $ PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeNameQualified @@ var "env" @@ var "tname")) @@ var "pargs",

      -- TermSet - encode as frozenset
      _Term_set>>: "s" ~>
        "pyEls" <<~ (Flows.mapList (var "encode") (Sets.toList $ var "s")) $
        produce $ PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ PyDsl.name (string "frozenset"))
          @@ list [PyUtils.pyAtomToPyExpression @@ PyDsl.atomSet (PyDsl.set (Lists.map PyUtils.pyExpressionToPyStarNamedExpression (var "pyEls")))],

      -- TermTypeApplication - strip type applications and potentially cast
      _Term_typeApplication>>: "ta" ~>
        "body" <~ Core.typeApplicationTermBody (var "ta") $
        "pybase" <<~ (encodeTermInline @@ var "env" @@ true @@ (var "stripTypeApps" @@ var "body")) $
        var "withCast" @@ var "pybase",

      -- TermTypeLambda - descend into body with updated environment
      _Term_typeLambda>>: "tl" ~>
        "body" <~ Core.typeLambdaBody (var "tl") $
        withTypeLambda @@ var "env" @@ var "tl" @@
          ("env2" ~> encodeTermInline @@ var "env2" @@ var "noCast" @@ var "body"),

      -- TermUnion (Injection)
      _Term_union>>: "inj" ~>
        "tname" <~ Core.injectionTypeName (var "inj") $
        "field" <~ Core.injectionField (var "inj") $
        "rt" <<~ (inGraphContext @@ (Schemas.requireUnionType @@ var "tname")) $
        Logic.ifElse (Schemas.isEnumRowType @@ var "rt")
          -- Enum variant
          (produce $ PyUtils.projectFromExpression
            @@ (PyUtils.pyNameToPyExpression @@ (PyNames.encodeNameQualified @@ var "env" @@ var "tname"))
            @@ (PyNames.encodeEnumValue @@ var "env" @@ Core.fieldName (var "field")))
          -- Class variant
          ("fname" <~ Core.fieldName (var "field") $
            "ftypes" <~ Core.rowTypeFields (var "rt") $
            -- Check if this is a unit variant
            "isUnitVariant" <~ (Maybes.maybe
              false
              ("ft" ~> Schemas.isUnitType @@ (Rewriting.deannotateType @@ Core.fieldTypeType (var "ft")))
              (Lists.find ("ft" ~> Core.equalName_ (Core.fieldTypeName (var "ft")) (var "fname")) (var "ftypes"))) $
            "args" <<~ (Logic.ifElse (Logic.or (Schemas.isUnitTerm @@ Core.fieldTerm (var "field")) (var "isUnitVariant"))
              (Flows.pure (list ([] :: [TTerm Py.Expression])))
              ("parg" <<~ (var "encode" @@ Core.fieldTerm (var "field")) $
                produce $ list [var "parg"])) $
            -- Cast to union type - set usesCast flag
            "unit_" <<~ (updateMeta @@ (setMetaUsesCast @@ true)) $
            produce $
              PyUtils.castTo
                @@ (PyNames.typeVariableReference @@ var "env" @@ var "tname")
                @@ (PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.variantName @@ true @@ var "env" @@ var "tname" @@ var "fname")) @@ var "args")),

      -- TermUnit
      _Term_unit>>: constant $
        produce $ PyUtils.pyNameToPyExpression @@ PyUtils.pyNone,

      -- TermVariable
      _Term_variable>>: "name" ~>
        encodeVariable @@ var "env" @@ var "name" @@ list ([] :: [TTerm Py.Expression]),

      -- TermWrap
      _Term_wrap>>: "wrapped" ~>
        "tname" <~ Core.wrappedTermTypeName (var "wrapped") $
        "inner" <~ Core.wrappedTermBody (var "wrapped") $
        "parg" <<~ (var "encode" @@ var "inner") $
        produce $ PyUtils.functionCall @@ (PyUtils.pyNameToPyPrimary @@ (PyNames.encodeNameQualified @@ var "env" @@ var "tname")) @@ list [var "parg"]]

-- | Extend metadata based on a term (used during module encoding).
--   Traverses a term and updates metadata flags based on what features are used.
--   The topLevel parameter affects whether we track function type annotations at this level.
extendMetaForTerm :: TBinding (Bool -> PyHelpers.PythonModuleMetadata -> Term -> PyHelpers.PythonModuleMetadata)
extendMetaForTerm = def "extendMetaForTerm" $
  doc "Extend metadata based on a term (used during module encoding)" $
  "topLevel" ~> "meta0" ~> "term" ~>
    "step" <~ ("meta" ~> "t" ~>
      cases _Term (var "t") (Just $ var "meta") [
        _Term_either>>: "e" ~>
          -- Either terms need cast() in Python for proper typing
          "metaWithCast" <~ (setMetaUsesCast @@ true @@ var "meta") $
          Eithers.either_
            (constant $ setMetaUsesLeft @@ var "metaWithCast" @@ true)
            (constant $ setMetaUsesRight @@ var "metaWithCast" @@ true)
            (var "e"),
        _Term_function>>: "f" ~>
          cases _Function (var "f") (Just $ var "meta") [
            _Function_lambda>>: "lam" ~>
              Maybes.maybe
                (var "meta")
                ("dom" ~> Logic.ifElse (var "topLevel")
                  (extendMetaForType @@ true @@ false @@ var "dom" @@ var "meta")
                  (var "meta"))
                (Core.lambdaDomain $ var "lam")],
        _Term_let>>: "lt" ~>
          "bindings" <~ Core.letBindings (var "lt") $
          Lists.foldl ("forBinding" <~ ("m" ~> "b" ~>
            Maybes.maybe
              (var "m")
              ("ts" ~>
                "term1" <~ Core.bindingTerm (var "b") $
                Logic.ifElse (CoderUtils.isSimpleAssignment @@ var "term1")
                  (var "m")
                  (extendMetaForType @@ true @@ true @@ (Core.typeSchemeType $ var "ts") @@ var "m"))
              (Core.bindingType $ var "b")) $
            var "forBinding") (var "meta") (var "bindings"),
        _Term_literal>>: "l" ~>
          cases _Literal (var "l") (Just $ var "meta") [
            _Literal_float>>: "fv" ~>
              cases _FloatValue (var "fv") (Just $ var "meta") [
                _FloatValue_bigfloat>>: constant $
                  setMetaUsesDecimal @@ var "meta" @@ true]],
        _Term_map>>: constant $
          setMetaUsesFrozenDict @@ var "meta" @@ true,
        _Term_maybe>>: "m" ~>
          Maybes.maybe
            (setMetaUsesNothing @@ var "meta" @@ true)
            (constant $ setMetaUsesJust @@ var "meta" @@ true)
            (var "m"),
        -- Union injections require cast() for proper typing
        _Term_union>>: constant $
          setMetaUsesCast @@ true @@ var "meta"]) $
    Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@ var "step" @@ var "meta0" @@ var "term"

-- Helper functions to set individual metadata fields

-- | Set the namespaces in metadata
setMetaNamespaces :: TBinding (Namespaces Py.DottedName -> PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata)
setMetaNamespaces = def "setMetaNamespaces" $
  "ns" ~> "m" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>: var "ns",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesLeft :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesLeft = def "setMetaUsesLeft" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>: var "b",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesRight :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesRight = def "setMetaUsesRight" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>: var "b",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesDecimal :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesDecimal = def "setMetaUsesDecimal" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>: var "b",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesFrozenDict :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesFrozenDict = def "setMetaUsesFrozenDict" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>: var "b",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesNothing :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesNothing = def "setMetaUsesNothing" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>: var "b",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesJust :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesJust = def "setMetaUsesJust" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>: var "b",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- Additional metadata setters

setMetaUsesCallable :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesCallable = def "setMetaUsesCallable" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>: var "b",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Set usesCast flag in metadata - used when generating cast() expressions for union types
setMetaUsesCast :: TBinding (Bool -> PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata)
setMetaUsesCast = def "setMetaUsesCast" $
  "b" ~> "m" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>: var "b",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Set usesLruCache flag in metadata - used when generating @lru_cache(1) for thunks
setMetaUsesLruCache :: TBinding (Bool -> PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata)
setMetaUsesLruCache = def "setMetaUsesLruCache" $
  "b" ~> "m" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>: var "b",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesGeneric :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesGeneric = def "setMetaUsesGeneric" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>: var "b",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesFrozenList :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesFrozenList = def "setMetaUsesFrozenList" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>: var "b",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesMaybe :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesMaybe = def "setMetaUsesMaybe" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>: var "b",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesEither :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesEither = def "setMetaUsesEither" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>: var "b",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesNode :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesNode = def "setMetaUsesNode" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>: var "b",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesEnum :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesEnum = def "setMetaUsesEnum" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>: var "b",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Recursively dig through forall types to find wrap types.
--   This is used to detect when we need to import Node for wrapped types
--   that are nested inside forall types (e.g., Flow = forall s. forall v. Wrap(...)).
digForWrap :: TBinding (Bool -> PyHelpers.PythonModuleMetadata -> Type -> PyHelpers.PythonModuleMetadata)
digForWrap = def "digForWrap" $
  doc "Recursively dig through forall types to find wrap types" $
  "isTermAnnot" ~> "meta" ~> "typ" ~>
    cases _Type (Rewriting.deannotateType @@ var "typ") (Just $ var "meta") [
      _Type_forall>>: "ft" ~>
        digForWrap @@ var "isTermAnnot" @@ var "meta" @@ Core.forallTypeBody (var "ft"),
      _Type_wrap>>: constant $
        Logic.ifElse (var "isTermAnnot")
          (var "meta")
          (setMetaUsesNode @@ var "meta" @@ true)]

-- | Extend metadata based on a type (used during module encoding).
--   topLevel: whether this is a top-level type annotation
--   isTermAnnot: whether this is a term's type annotation vs a type definition
extendMetaForType :: TBinding (Bool -> Bool -> Type -> PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata)
extendMetaForType = def "extendMetaForType" $
  doc "Extend metadata based on a type (used during module encoding)" $
  "topLevel" ~> "isTermAnnot" ~> "typ" ~> "meta" ~>
    -- First, collect type variables from this type
    "currentTvars" <~ (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "meta") $
    "newTvars" <~ (collectTypeVariables @@ var "currentTvars" @@ var "typ") $
    "metaWithTvars" <~ (setMetaTypeVariables @@ var "meta" @@ var "newTvars") $
    -- Then, extend metadata for subtypes recursively
    "metaWithSubtypes" <~ (Lists.foldl
      ("m" ~> "t" ~> extendMetaForType @@ false @@ var "isTermAnnot" @@ var "t" @@ var "m")
      (var "metaWithTvars")
      (Rewriting.subtypes @@ var "typ")) $
    -- Finally process this type for imports
    cases _Type (Rewriting.deannotateType @@ var "typ") (Just $ var "metaWithSubtypes") [
      -- Function type: may need Callable import
      _Type_function>>: "ft" ~>
        "cod" <~ Core.functionTypeCodomain (var "ft") $
        "dom" <~ Core.functionTypeDomain (var "ft") $
        "meta2" <~ (extendMetaForType @@ var "topLevel" @@ var "isTermAnnot" @@ var "cod" @@ var "metaWithSubtypes") $
        "meta3" <~ (extendMetaForType @@ false @@ var "isTermAnnot" @@ var "dom" @@ var "meta2") $
        Logic.ifElse (Logic.and (var "isTermAnnot") (var "topLevel"))
          (var "meta3")  -- Top-level function type on term: no Callable needed (def syntax)
          (setMetaUsesCallable @@ var "meta3" @@ true),  -- Otherwise need Callable
      -- List type: need frozenlist import
      _Type_list>>: constant $
        setMetaUsesFrozenList @@ var "metaWithSubtypes" @@ true,
      -- Map type: need FrozenDict import
      _Type_map>>: constant $
        setMetaUsesFrozenDict @@ var "metaWithSubtypes" @@ true,
      -- Maybe type: need Maybe import
      _Type_maybe>>: constant $
        setMetaUsesMaybe @@ var "metaWithSubtypes" @@ true,
      -- Either type: need Either import
      _Type_either>>: constant $
        setMetaUsesEither @@ var "metaWithSubtypes" @@ true,
      -- Literal type: check for Decimal
      _Type_literal>>: "lt" ~>
        cases _LiteralType (var "lt") (Just $ var "metaWithSubtypes") [
          _LiteralType_float>>: "ft" ~>
            cases _FloatType (var "ft") (Just $ var "metaWithSubtypes") [
              _FloatType_bigfloat>>: constant $
                setMetaUsesDecimal @@ var "metaWithSubtypes" @@ true]],
      -- Union type: need Enum or Node
      _Type_union>>: "rt" ~>
        Logic.ifElse (Schemas.isEnumRowType @@ var "rt")
          (setMetaUsesEnum @@ var "metaWithSubtypes" @@ true)
          (Logic.ifElse (Logic.not (Lists.null (Core.rowTypeFields $ var "rt")))
            (setMetaUsesNode @@ var "metaWithSubtypes" @@ true)
            (var "metaWithSubtypes")),
      -- Forall type: may need Generic for records, Node for wraps
      _Type_forall>>: "ft" ~>
        "body" <~ Core.forallTypeBody (var "ft") $
        -- Recursively check for wrap types (dig through nested foralls)
        "metaForWrap" <~ (digForWrap @@ var "isTermAnnot" @@ var "metaWithSubtypes" @@ var "body") $
        cases _Type (Rewriting.deannotateType @@ var "body") (Just $ var "metaForWrap") [
          _Type_record>>: constant $
            setMetaUsesGeneric @@ var "metaForWrap" @@ true],
      -- Record type: need dataclass (if non-empty) and possibly Annotated
      _Type_record>>: "rt" ~>
        "fields" <~ Core.rowTypeFields (var "rt") $
        -- Check if any field has a type description (needs Annotated)
        "hasAnnotated" <~ (Lists.foldl
          ("b" ~> "ft" ~> Logic.or (var "b") (Annotations.hasTypeDescription @@ Core.fieldTypeType (var "ft")))
          false
          (var "fields")) $
        -- Set usesDataclass if fields are non-empty
        "meta1" <~ (Logic.ifElse (Lists.null $ var "fields")
          (var "metaWithSubtypes")
          (setMetaUsesDataclass @@ var "metaWithSubtypes" @@ true)) $
        -- Set usesAnnotated if any field has description
        Logic.ifElse (var "hasAnnotated")
          (setMetaUsesAnnotated @@ var "meta1" @@ true)
          (var "meta1"),
      -- Wrap type: need Node import (unless it's a term annotation)
      _Type_wrap>>: constant $
        Logic.ifElse (var "isTermAnnot")
          (var "metaWithSubtypes")
          (setMetaUsesNode @@ var "metaWithSubtypes" @@ true)]

setMetaUsesAnnotated :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesAnnotated = def "setMetaUsesAnnotated" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>: var "b",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

setMetaUsesDataclass :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesDataclass = def "setMetaUsesDataclass" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>: var "b",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Set the type variables in metadata
setMetaTypeVariables :: TBinding (PyHelpers.PythonModuleMetadata -> S.Set Name -> PyHelpers.PythonModuleMetadata)
setMetaTypeVariables = def "setMetaTypeVariables" $
  "m" ~> "tvars" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>: var "tvars",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Check if a name is a type variable (unqualified - no dots)
isTypeVariableName :: TBinding (Name -> Bool)
isTypeVariableName = def "isTypeVariableName" $
  doc "Check if a name is a type variable (unqualified - no dots)" $
  "name" ~>
    -- Split on '.' and check if length is 1 (no dots means just one part)
    Equality.equal (int32 1) (Lists.length (Strings.splitOn (string ".") (Core.unName (var "name"))))

-- | Collect type variables from a type.
--   Collects both explicitly quantified variables (from forall) AND free type variables.
--   Filters out qualified names (those containing '.') since those are nominal types.
collectTypeVariables :: TBinding (S.Set Name -> Type -> S.Set Name)
collectTypeVariables = def "collectTypeVariables" $
  doc "Collect type variables from a type" $
  "initial" ~> "typ" ~>
    cases _Type (Rewriting.deannotateType @@ var "typ") (Just $
      -- Default: union initial with filtered free variables
      -- Filter free variables to only include unqualified names (type variables)
      "freeVars" <~ (Rewriting.freeVariablesInType @@ var "typ") $
      "isTypeVar" <~ ("n" ~> isTypeVariableName @@ var "n") $
      "filteredList" <~ Lists.filter (var "isTypeVar") (Sets.toList $ var "freeVars") $
      Sets.union (var "initial") (Sets.fromList $ var "filteredList")) [
      _Type_forall>>: "ft" ~>
        "v" <~ Core.forallTypeParameter (var "ft") $
        "body" <~ Core.forallTypeBody (var "ft") $
        collectTypeVariables @@ (Sets.insert (var "v") (var "initial")) @@ var "body"]

-- | Extend metadata for a list of types.
--   Collects dependency names and extends metadata for each type.
extendMetaForTypes :: TBinding ([Type] -> PyHelpers.PythonModuleMetadata -> PyHelpers.PythonModuleMetadata)
extendMetaForTypes = def "extendMetaForTypes" $
  doc "Extend metadata for a list of types" $
  "types" ~> "meta" ~>
    -- First compute names from all types
    "names" <~ Sets.unions (Lists.map ("t" ~> Rewriting.typeDependencyNames @@ false @@ var "t") (var "types")) $
    -- Update namespaces with the collected names
    "currentNs" <~ (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "meta") $
    "updatedNs" <~ (Schemas.addNamesToNamespaces @@ PyNames.encodeNamespace @@ var "names" @@ var "currentNs") $
    -- Create meta1 with updated namespaces
    "meta1" <~ (setMetaNamespaces @@ var "updatedNs" @@ var "meta") $
    -- Now fold extendMetaForType over all types with isTypeDef=True, isTermAnnot=False
    Lists.foldl ("m" ~> "t" ~> extendMetaForType @@ true @@ false @@ var "t" @@ var "m") (var "meta1") (var "types")

-- | Set usesName in metadata
setMetaUsesName :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesName = def "setMetaUsesName" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>: var "b",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Set usesTypeVar in metadata
setMetaUsesTypeVar :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesTypeVar = def "setMetaUsesTypeVar" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>: var "b"]

-- | Create an initial empty metadata record with given namespaces
emptyMetadata :: TBinding (Namespaces Py.DottedName -> PyHelpers.PythonModuleMetadata)
emptyMetadata = def "emptyMetadata" $
  doc "Create an initial empty metadata record with given namespaces" $
  "ns" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>: var "ns",
      PyHelpers._PythonModuleMetadata_typeVariables>>: Sets.empty,
      PyHelpers._PythonModuleMetadata_usesAnnotated>>: false,
      PyHelpers._PythonModuleMetadata_usesCallable>>: false,
      PyHelpers._PythonModuleMetadata_usesCast>>: false,
      PyHelpers._PythonModuleMetadata_usesLruCache>>: false,
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>: false,
      PyHelpers._PythonModuleMetadata_usesDataclass>>: false,
      PyHelpers._PythonModuleMetadata_usesDecimal>>: false,
      PyHelpers._PythonModuleMetadata_usesEither>>: false,
      PyHelpers._PythonModuleMetadata_usesEnum>>: false,
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>: false,
      PyHelpers._PythonModuleMetadata_usesFrozenList>>: false,
      PyHelpers._PythonModuleMetadata_usesGeneric>>: false,
      PyHelpers._PythonModuleMetadata_usesJust>>: false,
      PyHelpers._PythonModuleMetadata_usesLeft>>: false,
      PyHelpers._PythonModuleMetadata_usesMaybe>>: false,
      PyHelpers._PythonModuleMetadata_usesName>>: false,
      PyHelpers._PythonModuleMetadata_usesNode>>: false,
      PyHelpers._PythonModuleMetadata_usesNothing>>: false,
      PyHelpers._PythonModuleMetadata_usesRight>>: false,
      PyHelpers._PythonModuleMetadata_usesTypeVar>>: false]

-- | Gather metadata from a list of definitions.
--   This is the main entry point for collecting all import requirements.
gatherMetadata :: TBinding (Namespace -> [Definition] -> PyHelpers.PythonModuleMetadata)
gatherMetadata = def "gatherMetadata" $
  doc "Gather metadata from definitions" $
  "focusNs" ~> "defs" ~>
    -- Start with initial metadata containing namespaces
    "start" <~ (emptyMetadata @@ (PyUtils.findNamespaces @@ var "focusNs" @@ var "defs")) $
    -- Add function to extend metadata for each definition
    "addDef" <~ ("meta" ~> "def" ~>
      cases _Definition (var "def") Nothing [
        _Definition_term>>: "termDef" ~>
          "term" <~ Module.termDefinitionTerm (var "termDef") $
          "typScheme" <~ Module.termDefinitionType (var "termDef") $
          "typ" <~ Core.typeSchemeType (var "typScheme") $
          -- First extend for the type annotation (isTypeDef=True, isTermAnnot=True)
          "meta2" <~ (extendMetaForType @@ true @@ true @@ var "typ" @@ var "meta") $
          -- Then extend for the term body (isTopLevel=True)
          extendMetaForTerm @@ true @@ var "meta2" @@ var "term",
        _Definition_type>>: "typeDef" ~>
          "typ" <~ Module.typeDefinitionType (var "typeDef") $
          -- Set usesName=True for type definitions
          "meta2" <~ (setMetaUsesName @@ var "meta" @@ true) $
          -- Fold extendMetaForType over the type (isTypeDef=True, isTermAnnot=False)
          Rewriting.foldOverType @@ Coders.traversalOrderPre @@
            ("m" ~> "t" ~> extendMetaForType @@ true @@ false @@ var "t" @@ var "m") @@ var "meta2" @@ var "typ"]) $
    -- Fold over all definitions
    "result" <~ Lists.foldl (var "addDef") (var "start") (var "defs") $
    -- Check if we have type variables and set usesTypeVar accordingly
    "tvars" <~ (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "result") $
    setMetaUsesTypeVar @@ var "result" @@ (Logic.not $ Sets.null $ var "tvars")

-- | Set the usesTypeAlias flag in metadata
setMetaUsesTypeAlias :: TBinding (PyHelpers.PythonModuleMetadata -> Bool -> PyHelpers.PythonModuleMetadata)
setMetaUsesTypeAlias = def "setMetaUsesTypeAlias" $
  "m" ~> "b" ~>
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "m",
      PyHelpers._PythonModuleMetadata_typeVariables>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "m",
      PyHelpers._PythonModuleMetadata_usesAnnotated>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCallable>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "m",
      PyHelpers._PythonModuleMetadata_usesCast>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLruCache>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>: var "b",
      PyHelpers._PythonModuleMetadata_usesDataclass>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "m",
      PyHelpers._PythonModuleMetadata_usesDecimal>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEither>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "m",
      PyHelpers._PythonModuleMetadata_usesEnum>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "m",
      PyHelpers._PythonModuleMetadata_usesFrozenList>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "m",
      PyHelpers._PythonModuleMetadata_usesGeneric>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "m",
      PyHelpers._PythonModuleMetadata_usesJust>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "m",
      PyHelpers._PythonModuleMetadata_usesLeft>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "m",
      PyHelpers._PythonModuleMetadata_usesMaybe>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "m",
      PyHelpers._PythonModuleMetadata_usesName>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesName @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNode>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "m",
      PyHelpers._PythonModuleMetadata_usesNothing>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "m",
      PyHelpers._PythonModuleMetadata_usesRight>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "m",
      PyHelpers._PythonModuleMetadata_usesTypeVar>>:
        project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "m"]

-- | Check whether a list of definitions contains any type definitions
isTypeModuleCheck :: TBinding ([Definition] -> Bool)
isTypeModuleCheck = def "isTypeModuleCheck" $
  doc "Check whether a list of definitions contains any type definitions" $
  "defs" ~>
    Logic.not $ Lists.null $ Lists.filter
      ("d" ~> cases _Definition (var "d") (Just false) [
        _Definition_type>>: constant true])
      (var "defs")

-- | Reorder definitions: type definitions first (with Name first among types),
--   then term definitions in topological order
reorderDefs :: TBinding ([Definition] -> [Definition])
reorderDefs = def "reorderDefs" $
  doc "Reorder definitions: types first, then topologically sorted terms" $
  "defs" ~>
    "partitioned" <~ (Schemas.partitionDefinitions @@ var "defs") $
    "typeDefsRaw" <~ Pairs.first (var "partitioned") $
    "termDefsRaw" <~ Pairs.second (var "partitioned") $
    -- Sort type defs: Name type first, then the rest
    "nameFirst" <~ Lists.filter
      ("td" ~> Equality.equal
        (project _TypeDefinition _TypeDefinition_name @@ var "td")
        (wrap _Name $ string "hydra.core.Name"))
      (var "typeDefsRaw") $
    "nameRest" <~ Lists.filter
      ("td" ~> Logic.not $ Equality.equal
        (project _TypeDefinition _TypeDefinition_name @@ var "td")
        (wrap _Name $ string "hydra.core.Name"))
      (var "typeDefsRaw") $
    "sortedTypeDefs" <~ Lists.concat (list [
      Lists.map ("td" ~> inject _Definition _Definition_type (var "td")) (var "nameFirst"),
      Lists.map ("td" ~> inject _Definition _Definition_type (var "td")) (var "nameRest")]) $
    -- Sort term defs topologically
    "termDefs" <~ Lists.map ("td" ~> inject _Definition _Definition_term (var "td")) (var "termDefsRaw") $
    "sortedTermDefs" <~ (Lists.concat $ Sorting.topologicalSortNodes @@
      ("d" ~> cases _Definition (var "d") Nothing [
        _Definition_term>>: "td" ~> project _TermDefinition _TermDefinition_name @@ var "td"])
      @@
      ("d" ~> cases _Definition (var "d") (Just (list ([] :: [TTerm Name]))) [
        _Definition_term>>: "td" ~>
          Sets.toList $ Rewriting.freeVariablesInTerm @@ (project _TermDefinition _TermDefinition_term @@ var "td")])
      @@ var "termDefs") $
    Lists.concat (list [var "sortedTypeDefs", var "sortedTermDefs"])

-- | Create a TypeVar assignment statement for a type variable name
tvarStatement :: TBinding (Py.Name -> Py.Statement)
tvarStatement = def "tvarStatement" $
  doc "Create a TypeVar assignment statement for a type variable name" $
  "name" ~>
    PyUtils.assignmentStatement @@ var "name" @@
      (PyUtils.functionCall @@
        (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "TypeVar") @@
        list [PyUtils.doubleQuotedString @@ (unwrap Py._Name @@ var "name")])

-- | Conditionally include a symbol name based on a boolean flag
condImportSymbol :: TBinding (String -> Bool -> Maybe String)
condImportSymbol = def "condImportSymbol" $
  doc "Conditionally include a symbol name based on a boolean flag" $
  "name" ~> "flag" ~>
    Logic.ifElse (var "flag") (just $ var "name") nothing

-- | Generate domain import statements from namespace mappings
moduleDomainImports :: TBinding (Namespaces Py.DottedName -> [Py.ImportStatement])
moduleDomainImports = def "moduleDomainImports" $
  doc "Generate domain import statements from namespace mappings" $
  "namespaces" ~>
    "names" <~ (Lists.sort $ Maps.elems $ Module.namespacesMapping (var "namespaces")) $
    Lists.map
      ("ns" ~>
        inject Py._ImportStatement Py._ImportStatement_name
          (wrap Py._ImportName $ list [
            record Py._DottedAsName [
              Py._DottedAsName_name>>: var "ns",
              Py._DottedAsName_as>>: nothing]]))
      (var "names")

-- | Generate a single "from X import Y, Z" standard import statement
standardImportStatement :: TBinding (String -> [String] -> Py.ImportStatement)
standardImportStatement = def "standardImportStatement" $
  doc "Generate a single from-import statement" $
  "modName" ~> "symbols" ~>
    inject Py._ImportStatement Py._ImportStatement_from
      (record Py._ImportFrom [
        Py._ImportFrom_prefixes>>: list ([] :: [TTerm Py.RelativeImportPrefix]),
        Py._ImportFrom_dottedName>>: just (wrap Py._DottedName $ list [PyDsl.name $ var "modName"]),
        Py._ImportFrom_targets>>:
          inject Py._ImportFromTargets Py._ImportFromTargets_simple
            (Lists.map ("s" ~>
              record Py._ImportFromAsName [
                Py._ImportFromAsName_name>>: PyDsl.name $ var "s",
                Py._ImportFromAsName_as>>: nothing])
              (var "symbols"))])

-- | Generate standard import statements based on module metadata
moduleStandardImports :: TBinding (PyHelpers.PythonModuleMetadata -> [Py.ImportStatement])
moduleStandardImports = def "moduleStandardImports" $
  doc "Generate standard import statements based on module metadata" $
  "meta" ~>
    -- Build list of (moduleName, [Maybe symbol]) pairs, then filter to non-empty
    "pairs" <~ list [
      pair (string "__future__") (list [
        condImportSymbol @@ string "annotations" @@ PyNames.useFutureAnnotations]),
      pair (string "collections.abc") (list [
        condImportSymbol @@ string "Callable" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCallable @@ var "meta")]),
      pair (string "dataclasses") (list [
        condImportSymbol @@ string "dataclass" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDataclass @@ var "meta")]),
      pair (string "decimal") (list [
        condImportSymbol @@ string "Decimal" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesDecimal @@ var "meta")]),
      pair (string "enum") (list [
        condImportSymbol @@ string "Enum" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEnum @@ var "meta")]),
      pair (string "functools") (list [
        condImportSymbol @@ string "lru_cache" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLruCache @@ var "meta")]),
      pair (string "hydra.dsl.python") (list [
        condImportSymbol @@ string "Either" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesEither @@ var "meta"),
        condImportSymbol @@ string "FrozenDict" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenDict @@ var "meta"),
        condImportSymbol @@ string "Just" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesJust @@ var "meta"),
        condImportSymbol @@ string "Left" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesLeft @@ var "meta"),
        condImportSymbol @@ string "Maybe" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesMaybe @@ var "meta"),
        condImportSymbol @@ string "Node" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNode @@ var "meta"),
        condImportSymbol @@ string "Nothing" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesNothing @@ var "meta"),
        condImportSymbol @@ string "Right" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesRight @@ var "meta"),
        condImportSymbol @@ string "frozenlist" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesFrozenList @@ var "meta")]),
      pair (string "typing") (list [
        condImportSymbol @@ string "Annotated" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesAnnotated @@ var "meta"),
        condImportSymbol @@ string "Generic" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesGeneric @@ var "meta"),
        condImportSymbol @@ string "TypeAlias" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeAlias @@ var "meta"),
        condImportSymbol @@ string "TypeVar" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesTypeVar @@ var "meta"),
        condImportSymbol @@ string "cast" @@
          (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_usesCast @@ var "meta")])] $
    -- Filter each pair to remove Nothing symbols, then filter out pairs with no remaining symbols
    "simplified" <~ Maybes.cat (Lists.map
      ("p" ~>
        "modName" <~ Pairs.first (var "p") $
        "symbols" <~ Maybes.cat (Pairs.second (var "p")) $
        Logic.ifElse (Lists.null $ var "symbols")
          nothing
          (just $ pair (var "modName") (var "symbols")))
      (var "pairs")) $
    Lists.map
      ("p" ~> standardImportStatement @@ Pairs.first (var "p") @@ Pairs.second (var "p"))
      (var "simplified")

-- | Generate all import statements (standard + domain) for a Python module
moduleImports :: TBinding (Namespaces Py.DottedName -> PyHelpers.PythonModuleMetadata -> [Py.Statement])
moduleImports = def "moduleImports" $
  doc "Generate all import statements for a Python module" $
  "namespaces" ~> "meta" ~>
    Lists.map
      ("imp" ~> PyUtils.pySimpleStatementToPyStatement @@
        (PyDsl.simpleStatementImport $ var "imp"))
      (Lists.concat (list [
        moduleStandardImports @@ var "meta",
        moduleDomainImports @@ var "namespaces"]))

-- | Encode a Hydra module to a Python module AST.
--   This is the main orchestration function that:
--   1. Reorders definitions (types first, then topologically sorted terms)
--   2. Gathers initial metadata
--   3. Sets up the environment
--   4. Encodes all definitions
--   5. Generates imports based on metadata
--   6. Assembles the final module
encodePythonModule :: TBinding (Module -> [Definition] -> Flow Graph Py.Module)
encodePythonModule = def "encodePythonModule" $
  doc "Encode a Hydra module to a Python module AST" $
  "mod" ~> "defs0" ~>
    "defs" <~ (reorderDefs @@ var "defs0") $
    "meta0" <~ (gatherMetadata @@ (Module.moduleNamespace $ var "mod") @@ var "defs") $
    "g" <<~ Monads.getState $
    Monads.withState @@ (makePyGraph @@ var "g" @@ var "meta0") @@
      ("namespaces0" <~ (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "meta0") $
       "tcontext" <<~ (Inference.initialTypeContext @@ var "g") $
       "env0" <~ (initialEnvironment @@ var "namespaces0" @@ var "tcontext") $
       "isTypeMod" <~ (isTypeModuleCheck @@ var "defs0") $
       withDefinitions @@ var "env0" @@ var "defs" @@ ("env" ~>
         "defStmts" <<~ (Flows.map ("xs" ~> Lists.concat (var "xs")) (Flows.mapList ("d" ~> encodeDefinition @@ var "env" @@ var "d") (var "defs"))) $
         "pyg1" <<~ Monads.getState $
         "meta1" <~ (pyGraphMetadata @@ var "pyg1") $
         -- Adjust metadata: if not type module and useInlineTypeParams, clear usesTypeVar
         "meta2" <~ Logic.ifElse (Logic.and (Logic.not $ var "isTypeMod") (asTerm useInlineTypeParams))
           (setMetaUsesTypeVar @@ var "meta1" @@ false)
           (var "meta1") $
         -- Adjust metadata: if type module and Python 3.10, set usesTypeAlias
         "meta" <~ Logic.ifElse (Logic.and (var "isTypeMod")
           (Equality.equal (asTerm targetPythonVersion) (inject PyHelpers._PythonVersion PyHelpers._PythonVersion_python310 unit)))
           (setMetaUsesTypeAlias @@ var "meta2" @@ true)
           (var "meta2") $
         "namespaces" <~ (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_namespaces @@ var "meta1") $
         -- Generate comment statements from module description
         "commentStmts" <~ (Maybes.maybe
           (list ([] :: [TTerm Py.Statement]))
           ("c" ~> list [PyUtils.commentStatement @@ var "c"])
           (Maybes.map CoderUtils.normalizeComment (Module.moduleDescription $ var "mod"))) $
         -- Generate import statements
         "importStmts" <~ (moduleImports @@ var "namespaces" @@ var "meta") $
         -- Generate type variable statements
         "tvars" <~ Logic.ifElse (Logic.or (var "isTypeMod") (Logic.not $ asTerm useInlineTypeParams))
           (project PyHelpers._PythonModuleMetadata PyHelpers._PythonModuleMetadata_typeVariables @@ var "meta")
           Sets.empty $
         "tvarStmts" <~ Lists.map ("tv" ~> tvarStatement @@ (PyNames.encodeTypeVariable @@ var "tv")) (Sets.toList $ var "tvars") $
         -- Assemble final module body: filter out empty groups
         "body" <~ Lists.filter ("group" ~> Logic.not $ Lists.null $ var "group")
           (Lists.concat (list [
             list [var "commentStmts", var "importStmts", var "tvarStmts"],
             var "defStmts"])) $
         produce $ PyDsl.module_ (var "body")))

-- | Main entry point: convert a Hydra module to Python source files
moduleToPython :: TBinding (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
moduleToPython = def "moduleToPython" $
  doc "Convert a Hydra module to Python source files" $
  "mod" ~> "defs" ~>
    "file" <<~ (encodePythonModule @@ var "mod" @@ var "defs") $
    "s" <~ (Serialization.printExpr @@ (Serialization.parenthesize @@ (PySerde.encodeModule @@ var "file"))) $
    "path" <~ (Names.namespaceToFilePath @@ Util.caseConventionLowerSnake @@ (wrap _FileExtension $ string "py") @@ (Module.moduleNamespace $ var "mod")) $
    produce $ Maps.singleton (var "path") (var "s")
