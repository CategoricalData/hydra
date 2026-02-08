-- | Python serializer: converts Python AST to concrete syntax (source code).
-- Serializes the Python syntax model (Hydra.Ext.Python.Syntax) into properly formatted Python source code.

module Hydra.Ext.Sources.Python.Serde where

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
import Hydra.Ast
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Ext.Sources.Python.Syntax as PySyntax


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.python.serde"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, Serialization.ns]
    (PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python serializer: converts Python AST to concrete syntax"
  where
    elements = [
      toBinding encodeAnnotatedRhs,
      toBinding encodeAnnotatedStatement,
      toBinding encodeAnnotation,
      toBinding encodeArgs,
      toBinding encodeAssignment,
      toBinding encodeAssignmentExpression,
      toBinding encodeAtom,
      toBinding encodeAttribute,
      toBinding encodeAwaitPrimary,
      toBinding encodeBitwiseAnd,
      toBinding encodeBitwiseOr,
      toBinding encodeBitwiseXor,
      toBinding encodeBlock,
      toBinding encodeCapturePattern,
      toBinding encodeCaseBlock,
      toBinding encodeClassDefinition,
      toBinding encodeClassPattern,
      toBinding encodeClosedPattern,
      toBinding encodeComparison,
      toBinding encodeCompoundStatement,
      toBinding encodeConjunction,
      toBinding encodeDecorators,
      toBinding encodeDict,
      toBinding encodeDisjunction,
      toBinding encodeDottedAsName,
      toBinding encodeDottedName,
      toBinding encodeDoubleStarredKvpair,
      toBinding encodeExpression,
      toBinding encodeFactor,
      toBinding encodeFunctionDefRaw,
      toBinding encodeFunctionDefinition,
      toBinding encodeGroup,
      toBinding encodeGuard,
      toBinding encodeImportFrom,
      toBinding encodeImportFromAsName,
      toBinding encodeImportFromTargets,
      toBinding encodeImportName,
      toBinding encodeImportStatement,
      toBinding encodeInversion,
      toBinding encodeKeywordPattern,
      toBinding encodeKeywordPatterns,
      toBinding encodeKvpair,
      toBinding encodeKwarg,
      toBinding encodeKwargOrDoubleStarred,
      toBinding encodeKwargOrStarred,
      toBinding encodeLambda,
      toBinding encodeLambdaParamNoDefault,
      toBinding encodeLambdaParameters,
      toBinding encodeLambdaStarEtc,
      toBinding encodeList,
      toBinding encodeMatchStatement,
      toBinding encodeModule,
      toBinding encodeName,
      toBinding encodeNamedExpression,
      toBinding encodeNameOrAttribute,
      toBinding encodeNumber,
      toBinding encodeOrPattern,
      toBinding encodeParam,
      toBinding encodeParamNoDefault,
      toBinding encodeParamNoDefaultParameters,
      toBinding encodeParameters,
      toBinding encodePattern,
      toBinding encodePatternCaptureTarget,
      toBinding encodePatterns,
      toBinding encodePosArg,
      toBinding encodePositionalPatterns,
      toBinding encodePower,
      toBinding encodePrimary,
      toBinding encodePrimaryRhs,
      toBinding encodePrimaryWithRhs,
      toBinding encodeRaiseExpression,
      toBinding encodeRaiseStatement,
      toBinding encodeRelativeImportPrefix,
      toBinding encodeReturnStatement,
      toBinding encodeSet,
      toBinding encodeShiftExpression,
      toBinding encodeSimpleStatement,
      toBinding encodeSimpleTypeParameter,
      toBinding encodeSingleTarget,
      toBinding encodeSlice,
      toBinding encodeSliceOrStarredExpression,
      toBinding encodeSlices,
      toBinding encodeStarAtom,
      toBinding encodeStarExpression,
      toBinding encodeStarNamedExpression,
      toBinding encodeStarTarget,
      toBinding encodeStarredExpression,
      toBinding encodeStatement,
      toBinding encodeString,
      toBinding encodeSubjectExpression,
      toBinding encodeSum,
      toBinding encodeTerm_,
      toBinding encodeTargetWithStarAtom,
      toBinding encodeTuple,
      toBinding encodeTypeAlias,
      toBinding encodeTypeParameter,
      toBinding encodeTypedAssignment,
      toBinding encodeUntypedAssignment,
      toBinding encodeValuePattern,
      toBinding escapePythonString,
      toBinding toPythonComments]

-- =============================================================================
-- Core serialization
-- =============================================================================

encodeModule :: TBinding (Py.Module -> Expr)
encodeModule = def "encodeModule" $
  doc "Serialize a Python module to an AST expression" $
  lambda "mod" $ lets [
    "warning">: Serialization.cst @@ (toPythonComments @@ Constants.warningAutoGeneratedFile),
    "groups">: Lists.map
      (lambda "group" $ Serialization.newlineSep @@ Lists.map encodeStatement (var "group"))
      (unwrap Py._Module @@ var "mod")] $
    Serialization.doubleNewlineSep @@ Lists.cons (var "warning") (var "groups")

encodeAnnotatedStatement :: TBinding (Py.AnnotatedStatement -> Expr)
encodeAnnotatedStatement = def "encodeAnnotatedStatement" $
  doc "Serialize an annotated statement (with optional doc comment)" $
  lambda "as_" $ lets [
    "doc_">: project Py._AnnotatedStatement Py._AnnotatedStatement_comment @@ var "as_",
    "stmt">: project Py._AnnotatedStatement Py._AnnotatedStatement_statement @@ var "as_"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ (toPythonComments @@ var "doc_"),
      encodeStatement @@ var "stmt"]

encodeStatement :: TBinding (Py.Statement -> Expr)
encodeStatement = def "encodeStatement" $
  doc "Serialize a Python statement" $
  lambda "stmt" $
    cases Py._Statement (var "stmt") Nothing [
      Py._Statement_annotated>>: lambda "a" $ encodeAnnotatedStatement @@ var "a",
      Py._Statement_simple>>: lambda "ss" $
        Serialization.newlineSep @@ Lists.map encodeSimpleStatement (var "ss"),
      Py._Statement_compound>>: lambda "c" $ encodeCompoundStatement @@ var "c"]

encodeSimpleStatement :: TBinding (Py.SimpleStatement -> Expr)
encodeSimpleStatement = def "encodeSimpleStatement" $
  doc "Serialize a simple (single-line) Python statement" $
  lambda "ss" $
    cases Py._SimpleStatement (var "ss") Nothing [
      Py._SimpleStatement_assignment>>: lambda "a" $ encodeAssignment @@ var "a",
      Py._SimpleStatement_starExpressions>>: lambda "es" $
        Serialization.newlineSep @@ Lists.map encodeStarExpression (var "es"),
      Py._SimpleStatement_return>>: lambda "r" $ encodeReturnStatement @@ var "r",
      Py._SimpleStatement_raise>>: lambda "r" $ encodeRaiseStatement @@ var "r",
      Py._SimpleStatement_pass>>: constant $ Serialization.cst @@ string "pass",
      Py._SimpleStatement_break>>: constant $ Serialization.cst @@ string "break",
      Py._SimpleStatement_continue>>: constant $ Serialization.cst @@ string "continue",
      Py._SimpleStatement_import>>: lambda "i" $ encodeImportStatement @@ var "i",
      Py._SimpleStatement_typeAlias>>: lambda "t" $ encodeTypeAlias @@ var "t",
      Py._SimpleStatement_assert>>: lambda "_" $ Serialization.cst @@ string "assert ...",
      Py._SimpleStatement_global>>: lambda "_" $ Serialization.cst @@ string "global ...",
      Py._SimpleStatement_nonlocal>>: lambda "_" $ Serialization.cst @@ string "nonlocal ...",
      Py._SimpleStatement_del>>: lambda "_" $ Serialization.cst @@ string "del ..."]

encodeCompoundStatement :: TBinding (Py.CompoundStatement -> Expr)
encodeCompoundStatement = def "encodeCompoundStatement" $
  doc "Serialize a compound (multi-line) Python statement" $
  lambda "cs" $
    cases Py._CompoundStatement (var "cs") Nothing [
      Py._CompoundStatement_function>>: lambda "f" $ encodeFunctionDefinition @@ var "f",
      Py._CompoundStatement_if>>: lambda "_" $ Serialization.cst @@ string "if ...",
      Py._CompoundStatement_classDef>>: lambda "c" $ encodeClassDefinition @@ var "c",
      Py._CompoundStatement_with>>: lambda "_" $ Serialization.cst @@ string "with ...",
      Py._CompoundStatement_for>>: lambda "_" $ Serialization.cst @@ string "for ...",
      Py._CompoundStatement_try>>: lambda "_" $ Serialization.cst @@ string "try ...",
      Py._CompoundStatement_while>>: lambda "_" $ Serialization.cst @@ string "while ...",
      Py._CompoundStatement_match>>: lambda "m" $ encodeMatchStatement @@ var "m"]

-- =============================================================================
-- Expressions
-- =============================================================================

encodeExpression :: TBinding (Py.Expression -> Expr)
encodeExpression = def "encodeExpression" $
  doc "Serialize a Python expression" $
  lambda "expr" $
    cases Py._Expression (var "expr") Nothing [
      Py._Expression_simple>>: lambda "d" $ encodeDisjunction @@ var "d",
      Py._Expression_conditional>>: lambda "_" $ Serialization.cst @@ string "... if ... else ...",
      Py._Expression_lambda>>: lambda "l" $ encodeLambda @@ var "l"]

encodeDisjunction :: TBinding (Py.Disjunction -> Expr)
encodeDisjunction = def "encodeDisjunction" $
  doc "Serialize a disjunction (or expression)" $
  lambda "d" $
    Serialization.symbolSep @@ string "or" @@ Serialization.inlineStyle @@
      Lists.map encodeConjunction (unwrap Py._Disjunction @@ var "d")

encodeConjunction :: TBinding (Py.Conjunction -> Expr)
encodeConjunction = def "encodeConjunction" $
  doc "Serialize a conjunction (and expression)" $
  lambda "c" $
    Serialization.symbolSep @@ string "and" @@ Serialization.inlineStyle @@
      Lists.map encodeInversion (unwrap Py._Conjunction @@ var "c")

encodeInversion :: TBinding (Py.Inversion -> Expr)
encodeInversion = def "encodeInversion" $
  doc "Serialize an inversion (not expression)" $
  lambda "i" $
    cases Py._Inversion (var "i") Nothing [
      Py._Inversion_not>>: lambda "other" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "not", encodeInversion @@ var "other"],
      Py._Inversion_simple>>: lambda "c" $ encodeComparison @@ var "c"]

encodeComparison :: TBinding (Py.Comparison -> Expr)
encodeComparison = def "encodeComparison" $
  doc "Serialize a comparison expression" $
  lambda "cmp" $
    -- For now, just encode the LHS bitwise or; comparison operators are rarely used in generated code
    encodeBitwiseOr @@ (project Py._Comparison Py._Comparison_lhs @@ var "cmp")

encodeBitwiseOr :: TBinding (Py.BitwiseOr -> Expr)
encodeBitwiseOr = def "encodeBitwiseOr" $
  doc "Serialize a bitwise OR expression" $
  lambda "bor" $ lets [
    "lhs">: project Py._BitwiseOr Py._BitwiseOr_lhs @@ var "bor",
    "rhs">: project Py._BitwiseOr Py._BitwiseOr_rhs @@ var "bor"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [encodeBitwiseOr @@ var "l", Serialization.cst @@ string "|"])
        (var "lhs"),
      just $ encodeBitwiseXor @@ var "rhs"])

encodeBitwiseXor :: TBinding (Py.BitwiseXor -> Expr)
encodeBitwiseXor = def "encodeBitwiseXor" $
  doc "Serialize a bitwise XOR expression" $
  lambda "bxor" $ lets [
    "lhs">: project Py._BitwiseXor Py._BitwiseXor_lhs @@ var "bxor",
    "rhs">: project Py._BitwiseXor Py._BitwiseXor_rhs @@ var "bxor"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [encodeBitwiseXor @@ var "l", Serialization.cst @@ string "^"])
        (var "lhs"),
      just $ encodeBitwiseAnd @@ var "rhs"])

encodeBitwiseAnd :: TBinding (Py.BitwiseAnd -> Expr)
encodeBitwiseAnd = def "encodeBitwiseAnd" $
  doc "Serialize a bitwise AND expression" $
  lambda "band" $ lets [
    "lhs">: project Py._BitwiseAnd Py._BitwiseAnd_lhs @@ var "band",
    "rhs">: project Py._BitwiseAnd Py._BitwiseAnd_rhs @@ var "band"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [encodeBitwiseAnd @@ var "l", Serialization.cst @@ string "&"])
        (var "lhs"),
      just $ encodeShiftExpression @@ var "rhs"])

encodeShiftExpression :: TBinding (Py.ShiftExpression -> Expr)
encodeShiftExpression = def "encodeShiftExpression" $
  doc "Serialize a shift expression" $
  lambda "se" $
    -- Shift operators are rarely used; just encode the sum
    encodeSum @@ (project Py._ShiftExpression Py._ShiftExpression_rhs @@ var "se")

encodeSum :: TBinding (Py.Sum -> Expr)
encodeSum = def "encodeSum" $
  doc "Serialize a sum expression" $
  lambda "s" $
    -- Just encode the term for now; sum operators (+/-) rarely used in generated code
    encodeTerm_ @@ (project Py._Sum Py._Sum_rhs @@ var "s")

encodeTerm_ :: TBinding (Py.Term -> Expr)
encodeTerm_ = def "encodeTerm" $
  doc "Serialize a term expression" $
  lambda "t" $
    -- Just encode the factor; multiplication rarely used in generated code
    encodeFactor @@ (project Py._Term Py._Term_rhs @@ var "t")

encodeFactor :: TBinding (Py.Factor -> Expr)
encodeFactor = def "encodeFactor" $
  doc "Serialize a factor expression" $
  lambda "f" $
    cases Py._Factor (var "f") Nothing [
      Py._Factor_positive>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "+", encodeFactor @@ var "inner"],
      Py._Factor_negative>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "-", encodeFactor @@ var "inner"],
      Py._Factor_complement>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "~", encodeFactor @@ var "inner"],
      Py._Factor_simple>>: lambda "p" $ encodePower @@ var "p"]

encodePower :: TBinding (Py.Power -> Expr)
encodePower = def "encodePower" $
  doc "Serialize a power expression" $
  lambda "p" $ lets [
    "lhs">: project Py._Power Py._Power_lhs @@ var "p",
    "rhs">: project Py._Power Py._Power_rhs @@ var "p"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ encodeAwaitPrimary @@ var "lhs",
      Maybes.map (lambda "r" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "**", encodeFactor @@ var "r"])
        (var "rhs")])

encodeAwaitPrimary :: TBinding (Py.AwaitPrimary -> Expr)
encodeAwaitPrimary = def "encodeAwaitPrimary" $
  doc "Serialize an await primary expression" $
  lambda "ap" $ lets [
    "await_">: project Py._AwaitPrimary Py._AwaitPrimary_await @@ var "ap",
    "primary">: project Py._AwaitPrimary Py._AwaitPrimary_primary @@ var "ap"] $
    Logic.ifElse (var "await_")
      (Serialization.spaceSep @@ list [Serialization.cst @@ string "await", encodePrimary @@ var "primary"])
      (encodePrimary @@ var "primary")

encodePrimary :: TBinding (Py.Primary -> Expr)
encodePrimary = def "encodePrimary" $
  doc "Serialize a primary expression" $
  lambda "p" $
    cases Py._Primary (var "p") Nothing [
      Py._Primary_simple>>: lambda "a" $ encodeAtom @@ var "a",
      Py._Primary_compound>>: lambda "pwr" $ encodePrimaryWithRhs @@ var "pwr"]

encodePrimaryWithRhs :: TBinding (Py.PrimaryWithRhs -> Expr)
encodePrimaryWithRhs = def "encodePrimaryWithRhs" $
  doc "Serialize a primary with RHS" $
  lambda "pwr" $ lets [
    "prim">: project Py._PrimaryWithRhs Py._PrimaryWithRhs_primary @@ var "pwr",
    "rhs">: project Py._PrimaryWithRhs Py._PrimaryWithRhs_rhs @@ var "pwr"] $
    Serialization.noSep @@ list [encodePrimary @@ var "prim", encodePrimaryRhs @@ var "rhs"]

encodePrimaryRhs :: TBinding (Py.PrimaryRhs -> Expr)
encodePrimaryRhs = def "encodePrimaryRhs" $
  doc "Serialize a primary RHS" $
  lambda "rhs" $
    cases Py._PrimaryRhs (var "rhs") Nothing [
      Py._PrimaryRhs_call>>: lambda "args" $
        Serialization.noSep @@ list [Serialization.cst @@ string "(", encodeArgs @@ var "args", Serialization.cst @@ string ")"],
      Py._PrimaryRhs_project>>: lambda "name" $
        Serialization.noSep @@ list [Serialization.cst @@ string ".", encodeName @@ var "name"],
      Py._PrimaryRhs_slices>>: lambda "slices" $
        Serialization.noSep @@ list [Serialization.cst @@ string "[", encodeSlices @@ var "slices", Serialization.cst @@ string "]"],
      Py._PrimaryRhs_genexp>>: lambda "_" $ Serialization.cst @@ string "[...]"]

encodeAtom :: TBinding (Py.Atom -> Expr)
encodeAtom = def "encodeAtom" $
  doc "Serialize a Python atom (literal or basic expression)" $
  lambda "atom" $
    cases Py._Atom (var "atom") Nothing [
      Py._Atom_dict>>: lambda "d" $ encodeDict @@ var "d",
      Py._Atom_dictcomp>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._Atom_ellipsis>>: constant $ Serialization.cst @@ string "...",
      Py._Atom_false>>: constant $ Serialization.cst @@ string "False",
      Py._Atom_genexp>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._Atom_group>>: lambda "g" $ encodeGroup @@ var "g",
      Py._Atom_list>>: lambda "l" $ encodeList @@ var "l",
      Py._Atom_listcomp>>: lambda "_" $ Serialization.cst @@ string "[...]",
      Py._Atom_name>>: lambda "n" $ encodeName @@ var "n",
      Py._Atom_none>>: constant $ Serialization.cst @@ string "None",
      Py._Atom_number>>: lambda "n" $ encodeNumber @@ var "n",
      Py._Atom_set>>: lambda "s" $ encodeSet @@ var "s",
      Py._Atom_setcomp>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._Atom_string>>: lambda "s" $ encodeString @@ var "s",
      Py._Atom_true>>: constant $ Serialization.cst @@ string "True",
      Py._Atom_tuple>>: lambda "t" $ encodeTuple @@ var "t"]

encodeNamedExpression :: TBinding (Py.NamedExpression -> Expr)
encodeNamedExpression = def "encodeNamedExpression" $
  doc "Serialize a named expression" $
  lambda "ne" $
    cases Py._NamedExpression (var "ne") Nothing [
      Py._NamedExpression_simple>>: lambda "e" $ encodeExpression @@ var "e",
      Py._NamedExpression_assignment>>: lambda "ae" $ encodeAssignmentExpression @@ var "ae"]

encodeAssignmentExpression :: TBinding (Py.AssignmentExpression -> Expr)
encodeAssignmentExpression = def "encodeAssignmentExpression" $
  doc "Serialize an assignment expression (walrus operator)" $
  lambda "ae" $ lets [
    "name">: project Py._AssignmentExpression Py._AssignmentExpression_name @@ var "ae",
    "expr">: project Py._AssignmentExpression Py._AssignmentExpression_expression @@ var "ae"] $
    Serialization.spaceSep @@ list [
      encodeName @@ var "name",
      Serialization.cst @@ string ":=",
      encodeExpression @@ var "expr"]

-- =============================================================================
-- Names and Identifiers
-- =============================================================================

encodeName :: TBinding (Py.Name -> Expr)
encodeName = def "encodeName" $
  doc "Serialize a Python name/identifier" $
  lambda "n" $
    Serialization.cst @@ (unwrap Py._Name @@ var "n")

encodeDottedName :: TBinding (Py.DottedName -> Expr)
encodeDottedName = def "encodeDottedName" $
  doc "Serialize a dotted name (e.g., module.submodule)" $
  lambda "dn" $
    Serialization.cst @@ Strings.intercalate (string ".") (Lists.map (lambda "n" $ unwrap Py._Name @@ var "n") (unwrap Py._DottedName @@ var "dn"))

encodeAttribute :: TBinding (Py.Attribute -> Expr)
encodeAttribute = def "encodeAttribute" $
  doc "Serialize an attribute access" $
  lambda "attr" $
    Serialization.dotSep @@ Lists.map encodeName (unwrap Py._Attribute @@ var "attr")

encodeNameOrAttribute :: TBinding (Py.NameOrAttribute -> Expr)
encodeNameOrAttribute = def "encodeNameOrAttribute" $
  doc "Serialize a name or attribute" $
  lambda "noa" $
    Serialization.dotSep @@ Lists.map encodeName (unwrap Py._NameOrAttribute @@ var "noa")

-- =============================================================================
-- Literals
-- =============================================================================

encodeNumber :: TBinding (Py.Number -> Expr)
encodeNumber = def "encodeNumber" $
  doc "Serialize a Python number literal" $
  lambda "num" $
    cases Py._Number (var "num") Nothing [
      Py._Number_float>>: lambda "f" $ Serialization.cst @@ Literals.showFloat64 (var "f"),
      Py._Number_integer>>: lambda "i" $ Serialization.cst @@ Literals.showBigint (var "i")]

encodeString :: TBinding (Py.String_ -> Expr)
encodeString = def "encodeString" $
  doc "Serialize a Python string literal" $
  lambda "s" $ lets [
    "content">: project Py._String Py._String_value @@ var "s",
    "style">: project Py._String Py._String_quoteStyle @@ var "s"] $
    cases Py._QuoteStyle (var "style") Nothing [
      Py._QuoteStyle_single>>: constant $ Serialization.cst @@ (escapePythonString @@ false @@ var "content"),
      Py._QuoteStyle_double>>: constant $ Serialization.cst @@ (escapePythonString @@ true @@ var "content"),
      Py._QuoteStyle_triple>>: constant $ Serialization.noSep @@ list [
        Serialization.cst @@ string "r\"\"\"",
        Serialization.cst @@ var "content",
        Serialization.cst @@ string "\"\"\""]]

-- =============================================================================
-- Collections
-- =============================================================================

encodeList :: TBinding (Py.List -> Expr)
encodeList = def "encodeList" $
  doc "Serialize a Python list" $
  lambda "l" $
    Serialization.bracketListAdaptive @@ Lists.map encodeStarNamedExpression (unwrap Py._List @@ var "l")

encodeDict :: TBinding (Py.Dict -> Expr)
encodeDict = def "encodeDict" $
  doc "Serialize a Python dictionary" $
  lambda "d" $
    Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      Lists.map encodeDoubleStarredKvpair (unwrap Py._Dict @@ var "d")

encodeSet :: TBinding (Py.Set -> Expr)
encodeSet = def "encodeSet" $
  doc "Serialize a Python set" $
  lambda "s" $
    Serialization.bracesListAdaptive @@ Lists.map encodeStarNamedExpression (unwrap Py._Set @@ var "s")

encodeTuple :: TBinding (Py.Tuple -> Expr)
encodeTuple = def "encodeTuple" $
  doc "Serialize a Python tuple" $
  lambda "t" $ lets [
    "es">: unwrap Py._Tuple @@ var "t"] $
    Logic.ifElse (Equality.equal (Lists.length (var "es")) (int32 1))
      (Serialization.parens @@ (Serialization.noSep @@ list [
        encodeStarNamedExpression @@ Lists.head (var "es"),
        Serialization.cst @@ string ","]))
      (Serialization.parenList @@ false @@ Lists.map encodeStarNamedExpression (var "es"))

encodeGroup :: TBinding (Py.Group -> Expr)
encodeGroup = def "encodeGroup" $
  doc "Serialize a parenthesized group" $
  lambda "g" $
    cases Py._Group (var "g") Nothing [
      Py._Group_expression>>: lambda "ne" $ encodeNamedExpression @@ var "ne",
      Py._Group_yield>>: lambda "_" $ Serialization.cst @@ string "(yield ...)"]

encodeKvpair :: TBinding (Py.Kvpair -> Expr)
encodeKvpair = def "encodeKvpair" $
  doc "Serialize a key-value pair" $
  lambda "kv" $ lets [
    "k">: project Py._Kvpair Py._Kvpair_key @@ var "kv",
    "v">: project Py._Kvpair Py._Kvpair_value @@ var "kv"] $
    Serialization.spaceSep @@ list [
      Serialization.noSep @@ list [encodeExpression @@ var "k", Serialization.cst @@ string ":"],
      encodeExpression @@ var "v"]

encodeDoubleStarredKvpair :: TBinding (Py.DoubleStarredKvpair -> Expr)
encodeDoubleStarredKvpair = def "encodeDoubleStarredKvpair" $
  doc "Serialize a double-starred key-value pair" $
  lambda "dskv" $
    cases Py._DoubleStarredKvpair (var "dskv") Nothing [
      Py._DoubleStarredKvpair_pair>>: lambda "p" $ encodeKvpair @@ var "p",
      Py._DoubleStarredKvpair_starred>>: lambda "e" $
        Serialization.noSep @@ list [Serialization.cst @@ string "**", encodeBitwiseOr @@ var "e"]]

encodeStarNamedExpression :: TBinding (Py.StarNamedExpression -> Expr)
encodeStarNamedExpression = def "encodeStarNamedExpression" $
  doc "Serialize a star named expression" $
  lambda "sne" $
    cases Py._StarNamedExpression (var "sne") Nothing [
      Py._StarNamedExpression_star>>: lambda "bor" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", encodeBitwiseOr @@ var "bor"],
      Py._StarNamedExpression_simple>>: lambda "ne" $ encodeNamedExpression @@ var "ne"]

encodeStarExpression :: TBinding (Py.StarExpression -> Expr)
encodeStarExpression = def "encodeStarExpression" $
  doc "Serialize a star expression" $
  lambda "se" $
    cases Py._StarExpression (var "se") Nothing [
      Py._StarExpression_star>>: lambda "bor" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", encodeBitwiseOr @@ var "bor"],
      Py._StarExpression_simple>>: lambda "e" $ encodeExpression @@ var "e"]

encodeStarredExpression :: TBinding (Py.StarredExpression -> Expr)
encodeStarredExpression = def "encodeStarredExpression" $
  doc "Serialize a starred expression" $
  lambda "se" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "*",
      encodeExpression @@ (unwrap Py._StarredExpression @@ var "se")]

-- =============================================================================
-- Slices
-- =============================================================================

encodeSlices :: TBinding (Py.Slices -> Expr)
encodeSlices = def "encodeSlices" $
  doc "Serialize slices" $
  lambda "s" $ lets [
    "hd">: project Py._Slices Py._Slices_head @@ var "s",
    "tl">: project Py._Slices Py._Slices_tail @@ var "s"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.cons (encodeSlice @@ var "hd") (Lists.map encodeSliceOrStarredExpression (var "tl"))

encodeSlice :: TBinding (Py.Slice -> Expr)
encodeSlice = def "encodeSlice" $
  doc "Serialize a slice" $
  lambda "s" $
    cases Py._Slice (var "s") Nothing [
      Py._Slice_named>>: lambda "ne" $ encodeNamedExpression @@ var "ne",
      Py._Slice_slice>>: lambda "_" $ Serialization.cst @@ string ":"]

encodeSliceOrStarredExpression :: TBinding (Py.SliceOrStarredExpression -> Expr)
encodeSliceOrStarredExpression = def "encodeSliceOrStarredExpression" $
  doc "Serialize a slice or starred expression" $
  lambda "s" $
    cases Py._SliceOrStarredExpression (var "s") Nothing [
      Py._SliceOrStarredExpression_slice>>: lambda "sl" $ encodeSlice @@ var "sl",
      Py._SliceOrStarredExpression_starred>>: lambda "se" $ encodeStarredExpression @@ var "se"]

-- =============================================================================
-- Lambda
-- =============================================================================

encodeLambda :: TBinding (Py.Lambda -> Expr)
encodeLambda = def "encodeLambda" $
  doc "Serialize a lambda expression" $
  lambda "l" $ lets [
    "params">: project Py._Lambda Py._Lambda_params @@ var "l",
    "body">: project Py._Lambda Py._Lambda_body @@ var "l"] $
    Serialization.parens @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "lambda",
      Serialization.noSep @@ list [encodeLambdaParameters @@ var "params", Serialization.cst @@ string ":"],
      encodeExpression @@ var "body"])

encodeLambdaParameters :: TBinding (Py.LambdaParameters -> Expr)
encodeLambdaParameters = def "encodeLambdaParameters" $
  doc "Serialize lambda parameters" $
  lambda "lp" $ lets [
    "nodef">: project Py._LambdaParameters Py._LambdaParameters_paramNoDefault @@ var "lp"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map encodeLambdaParamNoDefault (var "nodef")

encodeLambdaParamNoDefault :: TBinding (Py.LambdaParamNoDefault -> Expr)
encodeLambdaParamNoDefault = def "encodeLambdaParamNoDefault" $
  doc "Serialize a lambda parameter without default" $
  lambda "p" $
    encodeName @@ (unwrap Py._LambdaParamNoDefault @@ var "p")

encodeLambdaStarEtc :: TBinding (Py.LambdaStarEtc -> Expr)
encodeLambdaStarEtc = def "encodeLambdaStarEtc" $
  doc "Serialize lambda star etc" $
  lambda "lse" $
    cases Py._LambdaStarEtc (var "lse") Nothing [
      Py._LambdaStarEtc_paramNoDefault>>: lambda "p" $ encodeLambdaParamNoDefault @@ var "p",
      Py._LambdaStarEtc_star>>: lambda "_" $ Serialization.cst @@ string "*...",
      Py._LambdaStarEtc_paramMaybeDefault>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._LambdaStarEtc_kwds>>: lambda "_" $ Serialization.cst @@ string "**..."]

-- =============================================================================
-- Function definitions
-- =============================================================================

encodeFunctionDefinition :: TBinding (Py.FunctionDefinition -> Expr)
encodeFunctionDefinition = def "encodeFunctionDefinition" $
  doc "Serialize a function definition" $
  lambda "fd" $ lets [
    "decs">: project Py._FunctionDefinition Py._FunctionDefinition_decorators @@ var "fd",
    "raw">: project Py._FunctionDefinition Py._FunctionDefinition_raw @@ var "fd"] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Maybes.map encodeDecorators (var "decs"),
      just $ encodeFunctionDefRaw @@ var "raw"])

encodeFunctionDefRaw :: TBinding (Py.FunctionDefRaw -> Expr)
encodeFunctionDefRaw = def "encodeFunctionDefRaw" $
  doc "Serialize a raw function definition" $
  lambda "fdr" $ lets [
    "async_">: project Py._FunctionDefRaw Py._FunctionDefRaw_async @@ var "fdr",
    "name">: project Py._FunctionDefRaw Py._FunctionDefRaw_name @@ var "fdr",
    "tparams">: project Py._FunctionDefRaw Py._FunctionDefRaw_typeParams @@ var "fdr",
    "params">: project Py._FunctionDefRaw Py._FunctionDefRaw_params @@ var "fdr",
    "retType">: project Py._FunctionDefRaw Py._FunctionDefRaw_returnType @@ var "fdr",
    "block">: project Py._FunctionDefRaw Py._FunctionDefRaw_block @@ var "fdr",
    "asyncKw">: Logic.ifElse (var "async_") (just $ Serialization.cst @@ string "async") nothing,
    "tparamPart">: Logic.ifElse (Lists.null (var "tparams"))
      nothing
      (just $ Serialization.bracketList @@ Serialization.inlineStyle @@ Lists.map encodeTypeParameter (var "tparams")),
    "paramPart">: Maybes.map encodeParameters (var "params"),
    "retPart">: Maybes.map (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "->", encodeExpression @@ var "t"]) (var "retType")] $
    Serialization.newlineSep @@ list [
      Serialization.noSep @@ list [
        Serialization.spaceSep @@ Maybes.cat (list [
          var "asyncKw",
          just $ Serialization.cst @@ string "def",
          just $ Serialization.noSep @@ Maybes.cat (list [
            just $ encodeName @@ var "name",
            var "tparamPart",
            just $ Serialization.cst @@ string "(",
            var "paramPart",
            just $ Serialization.cst @@ string ")"]),
          var "retPart"]),
        Serialization.cst @@ string ":"],
      encodeBlock @@ var "block"]

encodeDecorators :: TBinding (Py.Decorators -> Expr)
encodeDecorators = def "encodeDecorators" $
  doc "Serialize decorators" $
  lambda "decs" $
    Serialization.newlineSep @@ Lists.map
      (lambda "ne" $ Serialization.noSep @@ list [Serialization.cst @@ string "@", encodeNamedExpression @@ var "ne"])
      (unwrap Py._Decorators @@ var "decs")

encodeParameters :: TBinding (Py.Parameters -> Expr)
encodeParameters = def "encodeParameters" $
  doc "Serialize function parameters" $
  lambda "p" $
    cases Py._Parameters (var "p") Nothing [
      Py._Parameters_paramNoDefault>>: lambda "pnd" $ encodeParamNoDefaultParameters @@ var "pnd",
      Py._Parameters_slashNoDefault>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._Parameters_slashWithDefault>>: lambda "_" $ Serialization.cst @@ string "..."]

encodeParamNoDefaultParameters :: TBinding (Py.ParamNoDefaultParameters -> Expr)
encodeParamNoDefaultParameters = def "encodeParamNoDefaultParameters" $
  doc "Serialize parameters without defaults" $
  lambda "pndp" $ lets [
    "nodef">: project Py._ParamNoDefaultParameters Py._ParamNoDefaultParameters_paramNoDefault @@ var "pndp"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map encodeParamNoDefault (var "nodef")

encodeParamNoDefault :: TBinding (Py.ParamNoDefault -> Expr)
encodeParamNoDefault = def "encodeParamNoDefault" $
  doc "Serialize a parameter without default" $
  lambda "pnd" $
    encodeParam @@ (project Py._ParamNoDefault Py._ParamNoDefault_param @@ var "pnd")

encodeParam :: TBinding (Py.Param -> Expr)
encodeParam = def "encodeParam" $
  doc "Serialize a parameter" $
  lambda "p" $ lets [
    "name">: project Py._Param Py._Param_name @@ var "p",
    "ann">: project Py._Param Py._Param_annotation @@ var "p"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ encodeName @@ var "name",
      Maybes.map encodeAnnotation (var "ann")])

encodeAnnotation :: TBinding (Py.Annotation -> Expr)
encodeAnnotation = def "encodeAnnotation" $
  doc "Serialize a type annotation" $
  lambda "ann" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string ":",
      encodeExpression @@ (unwrap Py._Annotation @@ var "ann")]

-- =============================================================================
-- Class definitions
-- =============================================================================

encodeClassDefinition :: TBinding (Py.ClassDefinition -> Expr)
encodeClassDefinition = def "encodeClassDefinition" $
  doc "Serialize a class definition" $
  lambda "cd" $ lets [
    "decs">: project Py._ClassDefinition Py._ClassDefinition_decorators @@ var "cd",
    "name">: project Py._ClassDefinition Py._ClassDefinition_name @@ var "cd",
    "args">: project Py._ClassDefinition Py._ClassDefinition_arguments @@ var "cd",
    "body">: project Py._ClassDefinition Py._ClassDefinition_body @@ var "cd",
    "argPart">: Maybes.map (lambda "a" $ Serialization.noSep @@ list [
      Serialization.cst @@ string "(",
      encodeArgs @@ var "a",
      Serialization.cst @@ string ")"]) (var "args")] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Maybes.map encodeDecorators (var "decs"),
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "class", encodeName @@ var "name"],
        var "argPart",
        just $ Serialization.cst @@ string ":"]),
      just $ encodeBlock @@ var "body"])

-- =============================================================================
-- Blocks
-- =============================================================================

encodeBlock :: TBinding (Py.Block -> Expr)
encodeBlock = def "encodeBlock" $
  doc "Serialize a block" $
  lambda "b" $
    cases Py._Block (var "b") Nothing [
      Py._Block_indented>>: lambda "groups" $
        Serialization.tabIndentDoubleSpace @@ Lists.map
          (lambda "stmts" $ Serialization.newlineSep @@ Lists.map encodeStatement (var "stmts"))
          (var "groups"),
      Py._Block_simple>>: lambda "ss" $
        Serialization.semicolonSep @@ Lists.map encodeSimpleStatement (var "ss")]

-- =============================================================================
-- Imports
-- =============================================================================

encodeImportStatement :: TBinding (Py.ImportStatement -> Expr)
encodeImportStatement = def "encodeImportStatement" $
  doc "Serialize an import statement" $
  lambda "is_" $
    cases Py._ImportStatement (var "is_") Nothing [
      Py._ImportStatement_name>>: lambda "n" $ encodeImportName @@ var "n",
      Py._ImportStatement_from>>: lambda "f" $ encodeImportFrom @@ var "f"]

encodeImportName :: TBinding (Py.ImportName -> Expr)
encodeImportName = def "encodeImportName" $
  doc "Serialize an import name" $
  lambda "in_" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "import",
      Serialization.commaSep @@ Serialization.inlineStyle @@
        Lists.map encodeDottedAsName (unwrap Py._ImportName @@ var "in_")]

encodeImportFrom :: TBinding (Py.ImportFrom -> Expr)
encodeImportFrom = def "encodeImportFrom" $
  doc "Serialize an import from statement" $
  lambda "if_" $ lets [
    "prefixes">: project Py._ImportFrom Py._ImportFrom_prefixes @@ var "if_",
    "name">: project Py._ImportFrom Py._ImportFrom_dottedName @@ var "if_",
    "targets">: project Py._ImportFrom Py._ImportFrom_targets @@ var "if_",
    "lhs">: Serialization.noSep @@ Maybes.cat (
      Lists.concat (list [
        Lists.map (lambda "p" $ just $ encodeRelativeImportPrefix @@ var "p") (var "prefixes"),
        list [Maybes.map encodeDottedName (var "name")]]))] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "from",
      var "lhs",
      Serialization.cst @@ string "import",
      encodeImportFromTargets @@ var "targets"]

encodeImportFromTargets :: TBinding (Py.ImportFromTargets -> Expr)
encodeImportFromTargets = def "encodeImportFromTargets" $
  doc "Serialize import from targets" $
  lambda "t" $
    cases Py._ImportFromTargets (var "t") Nothing [
      Py._ImportFromTargets_simple>>: lambda "names" $
        Serialization.commaSep @@ Serialization.inlineStyle @@
          Lists.map encodeImportFromAsName (var "names"),
      Py._ImportFromTargets_parens>>: lambda "names" $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "(",
          Serialization.commaSep @@ Serialization.inlineStyle @@
            Lists.map encodeImportFromAsName (var "names"),
          Serialization.cst @@ string ")"],
      Py._ImportFromTargets_star>>: constant $ Serialization.cst @@ string "*"]

encodeImportFromAsName :: TBinding (Py.ImportFromAsName -> Expr)
encodeImportFromAsName = def "encodeImportFromAsName" $
  doc "Serialize an import from as name" $
  lambda "ifan" $ lets [
    "name">: project Py._ImportFromAsName Py._ImportFromAsName_name @@ var "ifan",
    "alias">: project Py._ImportFromAsName Py._ImportFromAsName_as @@ var "ifan"] $
    Maybes.maybe
      (encodeName @@ var "name")
      (lambda "a" $ Serialization.spaceSep @@ list [
        encodeName @@ var "name",
        Serialization.cst @@ string "as",
        encodeName @@ var "a"])
      (var "alias")

encodeDottedAsName :: TBinding (Py.DottedAsName -> Expr)
encodeDottedAsName = def "encodeDottedAsName" $
  doc "Serialize a dotted as name" $
  lambda "dan" $ lets [
    "name">: project Py._DottedAsName Py._DottedAsName_name @@ var "dan",
    "alias">: project Py._DottedAsName Py._DottedAsName_as @@ var "dan"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ encodeDottedName @@ var "name",
      Maybes.map (lambda "a" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "as", encodeName @@ var "a"]) (var "alias")])

encodeRelativeImportPrefix :: TBinding (Py.RelativeImportPrefix -> Expr)
encodeRelativeImportPrefix = def "encodeRelativeImportPrefix" $
  doc "Serialize a relative import prefix" $
  lambda "p" $
    cases Py._RelativeImportPrefix (var "p") Nothing [
      Py._RelativeImportPrefix_dot>>: constant $ Serialization.cst @@ string ".",
      Py._RelativeImportPrefix_ellipsis>>: constant $ Serialization.cst @@ string "..."]

-- =============================================================================
-- Assignments
-- =============================================================================

encodeAssignment :: TBinding (Py.Assignment -> Expr)
encodeAssignment = def "encodeAssignment" $
  doc "Serialize an assignment" $
  lambda "a" $
    cases Py._Assignment (var "a") Nothing [
      Py._Assignment_typed>>: lambda "t" $ encodeTypedAssignment @@ var "t",
      Py._Assignment_untyped>>: lambda "u" $ encodeUntypedAssignment @@ var "u",
      Py._Assignment_aug>>: lambda "_" $ Serialization.cst @@ string "... += ..."]

encodeTypedAssignment :: TBinding (Py.TypedAssignment -> Expr)
encodeTypedAssignment = def "encodeTypedAssignment" $
  doc "Serialize a typed assignment" $
  lambda "ta" $ lets [
    "lhs">: project Py._TypedAssignment Py._TypedAssignment_lhs @@ var "ta",
    "typ">: project Py._TypedAssignment Py._TypedAssignment_type @@ var "ta",
    "rhs">: project Py._TypedAssignment Py._TypedAssignment_rhs @@ var "ta"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.noSep @@ list [encodeSingleTarget @@ var "lhs", Serialization.cst @@ string ":"],
      just $ encodeExpression @@ var "typ",
      Maybes.map encodeAnnotatedRhs (var "rhs")])

encodeUntypedAssignment :: TBinding (Py.UntypedAssignment -> Expr)
encodeUntypedAssignment = def "encodeUntypedAssignment" $
  doc "Serialize an untyped assignment" $
  lambda "ua" $ lets [
    "targets">: project Py._UntypedAssignment Py._UntypedAssignment_targets @@ var "ua",
    "rhs">: project Py._UntypedAssignment Py._UntypedAssignment_rhs @@ var "ua"] $
    Serialization.spaceSep @@ Lists.concat (list [
      Lists.map encodeStarTarget (var "targets"),
      list [encodeAnnotatedRhs @@ var "rhs"]])

encodeAnnotatedRhs :: TBinding (Py.AnnotatedRhs -> Expr)
encodeAnnotatedRhs = def "encodeAnnotatedRhs" $
  doc "Serialize an annotated RHS" $
  lambda "arhs" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "=",
      cases Py._AnnotatedRhs (var "arhs") Nothing [
        Py._AnnotatedRhs_star>>: lambda "ses" $
          Serialization.commaSep @@ Serialization.inlineStyle @@
            Lists.map encodeStarExpression (var "ses"),
        Py._AnnotatedRhs_yield>>: lambda "_" $ Serialization.cst @@ string "yield ..."]]

encodeSingleTarget :: TBinding (Py.SingleTarget -> Expr)
encodeSingleTarget = def "encodeSingleTarget" $
  doc "Serialize a single target" $
  lambda "st" $
    cases Py._SingleTarget (var "st") Nothing [
      Py._SingleTarget_name>>: lambda "n" $ encodeName @@ var "n",
      Py._SingleTarget_parens>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._SingleTarget_subscriptAttributeTarget>>: lambda "_" $ Serialization.cst @@ string "..."]

encodeStarTarget :: TBinding (Py.StarTarget -> Expr)
encodeStarTarget = def "encodeStarTarget" $
  doc "Serialize a star target" $
  lambda "st" $
    cases Py._StarTarget (var "st") Nothing [
      Py._StarTarget_unstarred>>: lambda "t" $ encodeTargetWithStarAtom @@ var "t",
      Py._StarTarget_starred>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", encodeStarTarget @@ var "inner"]]

encodeTargetWithStarAtom :: TBinding (Py.TargetWithStarAtom -> Expr)
encodeTargetWithStarAtom = def "encodeTargetWithStarAtom" $
  doc "Serialize a target with star atom" $
  lambda "t" $
    cases Py._TargetWithStarAtom (var "t") Nothing [
      Py._TargetWithStarAtom_atom>>: lambda "a" $ encodeStarAtom @@ var "a",
      Py._TargetWithStarAtom_project>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._TargetWithStarAtom_slices>>: lambda "_" $ Serialization.cst @@ string "..."]

encodeStarAtom :: TBinding (Py.StarAtom -> Expr)
encodeStarAtom = def "encodeStarAtom" $
  doc "Serialize a star atom" $
  lambda "sa" $
    cases Py._StarAtom (var "sa") Nothing [
      Py._StarAtom_name>>: lambda "n" $ encodeName @@ var "n",
      Py._StarAtom_targetWithStarAtom>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._StarAtom_starTargetsTupleSeq>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._StarAtom_starTargetsListSeq>>: lambda "_" $ Serialization.cst @@ string "[...]"]

-- =============================================================================
-- Type alias
-- =============================================================================

encodeTypeAlias :: TBinding (Py.TypeAlias -> Expr)
encodeTypeAlias = def "encodeTypeAlias" $
  doc "Serialize a type alias" $
  lambda "ta" $ lets [
    "name">: project Py._TypeAlias Py._TypeAlias_name @@ var "ta",
    "tparams">: project Py._TypeAlias Py._TypeAlias_typeParams @@ var "ta",
    "expr">: project Py._TypeAlias Py._TypeAlias_expression @@ var "ta",
    "alias">: Serialization.noSep @@ Maybes.cat (list [
      just $ encodeName @@ var "name",
      Logic.ifElse (Lists.null (var "tparams"))
        nothing
        (just $ Serialization.bracketList @@ Serialization.inlineStyle @@ Lists.map encodeTypeParameter (var "tparams"))])] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "type",
      var "alias",
      Serialization.cst @@ string "=",
      encodeExpression @@ var "expr"]

encodeTypeParameter :: TBinding (Py.TypeParameter -> Expr)
encodeTypeParameter = def "encodeTypeParameter" $
  doc "Serialize a type parameter" $
  lambda "tp" $
    cases Py._TypeParameter (var "tp") Nothing [
      Py._TypeParameter_simple>>: lambda "s" $ encodeSimpleTypeParameter @@ var "s",
      Py._TypeParameter_star>>: lambda "_" $ Serialization.cst @@ string "*...",
      Py._TypeParameter_doubleStar>>: lambda "_" $ Serialization.cst @@ string "**..."]

encodeSimpleTypeParameter :: TBinding (Py.SimpleTypeParameter -> Expr)
encodeSimpleTypeParameter = def "encodeSimpleTypeParameter" $
  doc "Serialize a simple type parameter" $
  lambda "stp" $
    encodeName @@ (project Py._SimpleTypeParameter Py._SimpleTypeParameter_name @@ var "stp")

-- =============================================================================
-- Return and Raise
-- =============================================================================

encodeReturnStatement :: TBinding (Py.ReturnStatement -> Expr)
encodeReturnStatement = def "encodeReturnStatement" $
  doc "Serialize a return statement" $
  lambda "rs" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "return",
      Serialization.commaSep @@ Serialization.inlineStyle @@
        Lists.map encodeStarExpression (unwrap Py._ReturnStatement @@ var "rs")]

encodeRaiseStatement :: TBinding (Py.RaiseStatement -> Expr)
encodeRaiseStatement = def "encodeRaiseStatement" $
  doc "Serialize a raise statement" $
  lambda "rs" $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "raise",
      Maybes.map encodeRaiseExpression (unwrap Py._RaiseStatement @@ var "rs")])

encodeRaiseExpression :: TBinding (Py.RaiseExpression -> Expr)
encodeRaiseExpression = def "encodeRaiseExpression" $
  doc "Serialize a raise expression" $
  lambda "re" $ lets [
    "expr">: project Py._RaiseExpression Py._RaiseExpression_expression @@ var "re",
    "from_">: project Py._RaiseExpression Py._RaiseExpression_from @@ var "re"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ encodeExpression @@ var "expr",
      Maybes.map (lambda "f" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "from", encodeExpression @@ var "f"])
        (var "from_")])

-- =============================================================================
-- Match statements
-- =============================================================================

encodeMatchStatement :: TBinding (Py.MatchStatement -> Expr)
encodeMatchStatement = def "encodeMatchStatement" $
  doc "Serialize a match statement" $
  lambda "ms" $ lets [
    "subj">: project Py._MatchStatement Py._MatchStatement_subject @@ var "ms",
    "cases">: project Py._MatchStatement Py._MatchStatement_cases @@ var "ms"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "match",
        Serialization.noSep @@ list [encodeSubjectExpression @@ var "subj", Serialization.cst @@ string ":"]],
      Serialization.tabIndentDoubleSpace @@ Lists.map encodeCaseBlock (var "cases")]

encodeSubjectExpression :: TBinding (Py.SubjectExpression -> Expr)
encodeSubjectExpression = def "encodeSubjectExpression" $
  doc "Serialize a subject expression" $
  lambda "se" $
    cases Py._SubjectExpression (var "se") Nothing [
      Py._SubjectExpression_simple>>: lambda "ne" $ encodeNamedExpression @@ var "ne",
      Py._SubjectExpression_tuple>>: lambda "_" $ Serialization.cst @@ string "*..."]

encodeCaseBlock :: TBinding (Py.CaseBlock -> Expr)
encodeCaseBlock = def "encodeCaseBlock" $
  doc "Serialize a case block" $
  lambda "cb" $ lets [
    "patterns">: project Py._CaseBlock Py._CaseBlock_patterns @@ var "cb",
    "guard">: project Py._CaseBlock Py._CaseBlock_guard @@ var "cb",
    "body">: project Py._CaseBlock Py._CaseBlock_body @@ var "cb"] $
    Serialization.newlineSep @@ list [
      Serialization.noSep @@ list [
        Serialization.spaceSep @@ Maybes.cat (list [
          just $ Serialization.cst @@ string "case",
          just $ encodePatterns @@ var "patterns",
          Maybes.map encodeGuard (var "guard")]),
        Serialization.cst @@ string ":"],
      encodeBlock @@ var "body"]

encodeGuard :: TBinding (Py.Guard -> Expr)
encodeGuard = def "encodeGuard" $
  doc "Serialize a guard clause" $
  lambda "g" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      encodeNamedExpression @@ (unwrap Py._Guard @@ var "g")]

encodePatterns :: TBinding (Py.Patterns -> Expr)
encodePatterns = def "encodePatterns" $
  doc "Serialize patterns" $
  lambda "ps" $
    cases Py._Patterns (var "ps") Nothing [
      Py._Patterns_pattern>>: lambda "p" $ encodePattern @@ var "p",
      Py._Patterns_sequence>>: lambda "_" $ Serialization.cst @@ string "..."]

encodePattern :: TBinding (Py.Pattern -> Expr)
encodePattern = def "encodePattern" $
  doc "Serialize a pattern" $
  lambda "p" $
    cases Py._Pattern (var "p") Nothing [
      Py._Pattern_or>>: lambda "op" $ encodeOrPattern @@ var "op",
      Py._Pattern_as>>: lambda "_" $ Serialization.cst @@ string "... as ..."]

encodeOrPattern :: TBinding (Py.OrPattern -> Expr)
encodeOrPattern = def "encodeOrPattern" $
  doc "Serialize an or pattern" $
  lambda "op" $
    Serialization.symbolSep @@ string "|" @@ Serialization.inlineStyle @@
      Lists.map encodeClosedPattern (unwrap Py._OrPattern @@ var "op")

encodeClosedPattern :: TBinding (Py.ClosedPattern -> Expr)
encodeClosedPattern = def "encodeClosedPattern" $
  doc "Serialize a closed pattern" $
  lambda "cp" $
    cases Py._ClosedPattern (var "cp") Nothing [
      Py._ClosedPattern_literal>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._ClosedPattern_capture>>: lambda "c" $ encodeCapturePattern @@ var "c",
      Py._ClosedPattern_wildcard>>: constant $ Serialization.cst @@ string "_",
      Py._ClosedPattern_value>>: lambda "v" $ encodeValuePattern @@ var "v",
      Py._ClosedPattern_group>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._ClosedPattern_sequence>>: lambda "_" $ Serialization.cst @@ string "[...]",
      Py._ClosedPattern_mapping>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._ClosedPattern_class>>: lambda "c" $ encodeClassPattern @@ var "c"]

encodeCapturePattern :: TBinding (Py.CapturePattern -> Expr)
encodeCapturePattern = def "encodeCapturePattern" $
  doc "Serialize a capture pattern" $
  lambda "cp" $
    encodePatternCaptureTarget @@ (unwrap Py._CapturePattern @@ var "cp")

encodePatternCaptureTarget :: TBinding (Py.PatternCaptureTarget -> Expr)
encodePatternCaptureTarget = def "encodePatternCaptureTarget" $
  doc "Serialize a pattern capture target" $
  lambda "pct" $
    encodeName @@ (unwrap Py._PatternCaptureTarget @@ var "pct")

encodeValuePattern :: TBinding (Py.ValuePattern -> Expr)
encodeValuePattern = def "encodeValuePattern" $
  doc "Serialize a value pattern" $
  lambda "vp" $
    encodeAttribute @@ (unwrap Py._ValuePattern @@ var "vp")

encodeClassPattern :: TBinding (Py.ClassPattern -> Expr)
encodeClassPattern = def "encodeClassPattern" $
  doc "Serialize a class pattern" $
  lambda "cp" $ lets [
    "noa">: project Py._ClassPattern Py._ClassPattern_nameOrAttribute @@ var "cp",
    "pos">: project Py._ClassPattern Py._ClassPattern_positionalPatterns @@ var "cp",
    "kw">: project Py._ClassPattern Py._ClassPattern_keywordPatterns @@ var "cp"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ encodeNameOrAttribute @@ var "noa",
      just $ Serialization.cst @@ string "(",
      Maybes.map encodePositionalPatterns (var "pos"),
      Maybes.map encodeKeywordPatterns (var "kw"),
      just $ Serialization.cst @@ string ")"])

encodePositionalPatterns :: TBinding (Py.PositionalPatterns -> Expr)
encodePositionalPatterns = def "encodePositionalPatterns" $
  doc "Serialize positional patterns" $
  lambda "pp" $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map encodePattern (unwrap Py._PositionalPatterns @@ var "pp")

encodeKeywordPatterns :: TBinding (Py.KeywordPatterns -> Expr)
encodeKeywordPatterns = def "encodeKeywordPatterns" $
  doc "Serialize keyword patterns" $
  lambda "kp" $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map encodeKeywordPattern (unwrap Py._KeywordPatterns @@ var "kp")

encodeKeywordPattern :: TBinding (Py.KeywordPattern -> Expr)
encodeKeywordPattern = def "encodeKeywordPattern" $
  doc "Serialize a keyword pattern" $
  lambda "kp" $ lets [
    "name">: project Py._KeywordPattern Py._KeywordPattern_name @@ var "kp",
    "pat">: project Py._KeywordPattern Py._KeywordPattern_pattern @@ var "kp"] $
    Serialization.noSep @@ list [
      encodeName @@ var "name",
      Serialization.cst @@ string "=",
      encodePattern @@ var "pat"]

-- =============================================================================
-- Arguments
-- =============================================================================

encodeArgs :: TBinding (Py.Args -> Expr)
encodeArgs = def "encodeArgs" $
  doc "Serialize function arguments" $
  lambda "args" $ lets [
    "pos">: project Py._Args Py._Args_positional @@ var "args",
    "ks">: project Py._Args Py._Args_kwargOrStarred @@ var "args",
    "kss">: project Py._Args Py._Args_kwargOrDoubleStarred @@ var "args"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.concat (list [
      Lists.map encodePosArg (var "pos"),
      Lists.map encodeKwargOrStarred (var "ks"),
      Lists.map encodeKwargOrDoubleStarred (var "kss")])

encodePosArg :: TBinding (Py.PosArg -> Expr)
encodePosArg = def "encodePosArg" $
  doc "Serialize a positional argument" $
  lambda "pa" $
    cases Py._PosArg (var "pa") Nothing [
      Py._PosArg_starred>>: lambda "se" $ encodeStarredExpression @@ var "se",
      Py._PosArg_assignment>>: lambda "ae" $ encodeAssignmentExpression @@ var "ae",
      Py._PosArg_expression>>: lambda "e" $ encodeExpression @@ var "e"]

encodeKwargOrStarred :: TBinding (Py.KwargOrStarred -> Expr)
encodeKwargOrStarred = def "encodeKwargOrStarred" $
  doc "Serialize a kwarg or starred" $
  lambda "ks" $
    cases Py._KwargOrStarred (var "ks") Nothing [
      Py._KwargOrStarred_kwarg>>: lambda "k" $ encodeKwarg @@ var "k",
      Py._KwargOrStarred_starred>>: lambda "se" $ encodeStarredExpression @@ var "se"]

encodeKwargOrDoubleStarred :: TBinding (Py.KwargOrDoubleStarred -> Expr)
encodeKwargOrDoubleStarred = def "encodeKwargOrDoubleStarred" $
  doc "Serialize a kwarg or double starred" $
  lambda "kds" $
    cases Py._KwargOrDoubleStarred (var "kds") Nothing [
      Py._KwargOrDoubleStarred_kwarg>>: lambda "k" $ encodeKwarg @@ var "k",
      Py._KwargOrDoubleStarred_doubleStarred>>: lambda "e" $
        Serialization.noSep @@ list [Serialization.cst @@ string "**", encodeExpression @@ var "e"]]

encodeKwarg :: TBinding (Py.Kwarg -> Expr)
encodeKwarg = def "encodeKwarg" $
  doc "Serialize a keyword argument" $
  lambda "k" $ lets [
    "name">: project Py._Kwarg Py._Kwarg_name @@ var "k",
    "expr">: project Py._Kwarg Py._Kwarg_value @@ var "k"] $
    Serialization.noSep @@ list [
      encodeName @@ var "name",
      Serialization.cst @@ string "=",
      encodeExpression @@ var "expr"]

-- =============================================================================
-- Utility functions
-- =============================================================================

escapePythonString :: TBinding (Bool -> String -> String)
escapePythonString = def "escapePythonString" $
  doc "Escape special characters in a Python string and wrap in quotes" $
  lambda "doubleQuoted" $ lambda "s" $ lets [
    -- Helper to replace a substring
    "replace">: lambda "old" $ lambda "new" $ lambda "str" $
      Strings.intercalate (var "new") (Strings.splitOn (var "old") (var "str")),
    -- Escape backslashes first (must be first!)
    "s1">: var "replace" @@ string "\\" @@ string "\\\\" @@ var "s",
    -- Escape null bytes
    "s2">: var "replace" @@ string "\0" @@ string "\\x00" @@ var "s1",
    -- Escape newlines
    "s3">: var "replace" @@ string "\n" @@ string "\\n" @@ var "s2",
    -- Escape tabs
    "s4">: var "replace" @@ string "\t" @@ string "\\t" @@ var "s3",
    -- Escape carriage returns
    "s5">: var "replace" @@ string "\r" @@ string "\\r" @@ var "s4",
    -- Escape quotes based on quote style
    "escaped">: Logic.ifElse (var "doubleQuoted")
      (var "replace" @@ string "\"" @@ string "\\\"" @@ var "s5")
      (var "replace" @@ string "'" @@ string "\\'" @@ var "s5"),
    -- Add surrounding quotes
    "quote">: Logic.ifElse (var "doubleQuoted") (string "\"") (string "'")] $
    Strings.cat2 (var "quote") (Strings.cat2 (var "escaped") (var "quote"))

toPythonComments :: TBinding (String -> String)
toPythonComments = def "toPythonComments" $
  doc "Convert a doc string to Python comment format" $
  lambda "doc_" $
    Logic.ifElse (Equality.equal (var "doc_") (string ""))
      (string "")
      (Strings.intercalate (string "\n") (Lists.map (lambda "line" $ Strings.cat2 (string "# ") (var "line")) (Strings.lines (var "doc_"))))
