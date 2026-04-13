-- | Python utilities for constructing Python syntax trees.
-- Provides functions for building common Python AST patterns.

module Hydra.Sources.Python.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Analysis      as Analysis
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
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
import qualified Hydra.Python.Syntax as Py
import qualified Hydra.Python.Environment as PyHelpers
import qualified Hydra.Sources.Python.Syntax as PySyntax
import qualified Hydra.Sources.Python.Environment as PyEnvironmentSource
import qualified Hydra.Sources.Python.Serde as PySerde
import qualified Hydra.Sources.Python.Names as PyNames
import qualified Hydra.Dsl.Python.Helpers as PyDsl


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.python.utils"

module_ :: Module
module_ = Module ns definitions
    [PyNames.ns, PySerde.ns, Serialization.ns, Analysis.ns]
    (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python utilities for constructing Python syntax trees"
  where
    definitions = [
      toDefinition annotatedExpression,
      toDefinition annotatedStatement,
      toDefinition assignment,
      toDefinition assignmentStatement,
      toDefinition castTo,
      toDefinition commentStatement,
      toDefinition decodePyComparisonToPyAwaitPrimary,
      toDefinition decodePyConjunctionToPyPrimary,
      toDefinition decodePyExpressionToPyPrimary,
      toDefinition decodePyInversionToPyPrimary,
      toDefinition decodePyPowerToPyPrimary,
      toDefinition dottedAssignmentStatement,
      toDefinition doubleQuotedString,
      toDefinition findNamespaces,
      toDefinition functionCall,
      toDefinition getItemParams,
      toDefinition indentedBlock,
      toDefinition nameAndParams,
      toDefinition newtypeStatement,
      toDefinition orExpression,
      toDefinition primaryAndParams,
      toDefinition primaryWithExpressionSlices,
      toDefinition primaryWithRhs,
      toDefinition primaryWithSlices,
      toDefinition projectFromExpression,
      toDefinition pyAssignmentToPyStatement,
      toDefinition pyAtomToPyExpression,
      toDefinition pyBitwiseOrToPyConjunction,
      toDefinition pyBitwiseOrToPyExpression,
      toDefinition pyClassDefinitionToPyStatement,
      toDefinition pyClosedPatternToPyPatterns,
      toDefinition pyConjunctionToPyExpression,
      toDefinition pyExpressionsToPyArgs,
      toDefinition pyExpressionToBitwiseOr,
      toDefinition pyExpressionToDisjunction,
      toDefinition pyExpressionToPyAnnotatedRhs,
      toDefinition pyExpressionToPyPrimary,
      toDefinition pyExpressionToPySimpleStatement,
      toDefinition pyExpressionToPySlice,
      toDefinition pyExpressionToPyStarNamedExpression,
      toDefinition pyExpressionToPyStatement,
      toDefinition pyList,
      toDefinition pyNameToPyExpression,
      toDefinition pyNameToPyNamedExpression,
      toDefinition pyNameToPyPrimary,
      toDefinition pyNameToPyStarTarget,
      toDefinition pyNameToPyTypeParameter,
      toDefinition pyNone,
      toDefinition pyPrimaryToPyBitwiseOr,
      toDefinition pyPrimaryToPyBitwiseXor,
      toDefinition pyPrimaryToPyConjunction,
      toDefinition pyPrimaryToPyExpression,
      toDefinition pyPrimaryToPySlice,
      toDefinition pySimpleStatementToPyStatement,
      toDefinition raiseAssertionError,
      toDefinition raiseTypeError,
      toDefinition returnSingle,
      toDefinition selfOnlyParams,
      toDefinition selfOtherParams,
      toDefinition singleQuotedString,
      toDefinition stringToPyExpression,
      toDefinition targetPythonVersion,
      toDefinition tripleQuotedString,
      toDefinition typeAliasStatement,
      toDefinition typeAliasStatement310,
      toDefinition unionTypeClassStatements310,
      toDefinition unitVariantMethods]

-- | Current target Python version. Change this to Python310 for PyPy compatibility.
targetPythonVersion :: TTermDefinition PyHelpers.PythonVersion
targetPythonVersion = def "targetPythonVersion" $
  doc "Current target Python version for code generation" $
  injectUnit PyHelpers._PythonVersion PyHelpers._PythonVersion_python310

-- | The Python None value as a Name
pyNone :: TTermDefinition Py.Name
pyNone = def "pyNone" $
  doc "The Python None value as a Name" $
  PyDsl.name $ string "None"

-- | Convert a Name to a Primary (simple atom)
pyNameToPyPrimary :: TTermDefinition (Py.Name -> Py.Primary)
pyNameToPyPrimary = def "pyNameToPyPrimary" $
  doc "Convert a Name to a Primary (simple atom)" $
  lambda "name" $ PyDsl.primarySimple $ PyDsl.atomName $ var "name"

-- | Convert a Primary to a BitwiseXor
pyPrimaryToPyBitwiseXor :: TTermDefinition (Py.Primary -> Py.BitwiseXor)
pyPrimaryToPyBitwiseXor = def "pyPrimaryToPyBitwiseXor" $
  doc "Convert a Primary to a BitwiseXor" $
  "prim" ~> PyDsl.pyPrimaryToPyBitwiseXor (var "prim")

-- | Convert a Primary to a BitwiseOr
pyPrimaryToPyBitwiseOr :: TTermDefinition (Py.Primary -> Py.BitwiseOr)
pyPrimaryToPyBitwiseOr = def "pyPrimaryToPyBitwiseOr" $
  doc "Convert a Primary to a BitwiseOr" $
  "prim" ~> PyDsl.pyPrimaryToPyBitwiseOr (var "prim")

-- | Convert a BitwiseOr to a Conjunction
pyBitwiseOrToPyConjunction :: TTermDefinition (Py.BitwiseOr -> Py.Conjunction)
pyBitwiseOrToPyConjunction = def "pyBitwiseOrToPyConjunction" $
  doc "Convert a BitwiseOr to a Conjunction" $
  lambda "bor" $
    PyDsl.conjunction $ list [
      PyDsl.inversionSimple $
        PyDsl.comparison (var "bor") (list ([] :: [TTerm Py.CompareOpBitwiseOrPair]))]

-- | Convert a Primary to a Conjunction
pyPrimaryToPyConjunction :: TTermDefinition (Py.Primary -> Py.Conjunction)
pyPrimaryToPyConjunction = def "pyPrimaryToPyConjunction" $
  doc "Convert a Primary to a Conjunction" $
  lambda "prim" $
    pyBitwiseOrToPyConjunction @@ (pyPrimaryToPyBitwiseOr @@ var "prim")

-- | Convert a Conjunction to an Expression
pyConjunctionToPyExpression :: TTermDefinition (Py.Conjunction -> Py.Expression)
pyConjunctionToPyExpression = def "pyConjunctionToPyExpression" $
  doc "Convert a Conjunction to an Expression" $
  lambda "conj" $
    PyDsl.expressionSimple $ PyDsl.disjunction $ list [var "conj"]

-- | Convert a Primary to an Expression
pyPrimaryToPyExpression :: TTermDefinition (Py.Primary -> Py.Expression)
pyPrimaryToPyExpression = def "pyPrimaryToPyExpression" $
  doc "Convert a Primary to an Expression" $
  lambda "prim" $
    pyConjunctionToPyExpression @@ (pyPrimaryToPyConjunction @@ var "prim")

-- | Convert an Atom to an Expression
pyAtomToPyExpression :: TTermDefinition (Py.Atom -> Py.Expression)
pyAtomToPyExpression = def "pyAtomToPyExpression" $
  doc "Convert an Atom to an Expression" $
  lambda "atom" $
    pyPrimaryToPyExpression @@ (PyDsl.primarySimple $ var "atom")

-- | Convert a Name to an Expression
pyNameToPyExpression :: TTermDefinition (Py.Name -> Py.Expression)
pyNameToPyExpression = def "pyNameToPyExpression" $
  doc "Convert a Name to an Expression" $
  lambda "name" $
    pyPrimaryToPyExpression @@ (pyNameToPyPrimary @@ var "name")

-- | Convert a SimpleStatement to a Statement
pySimpleStatementToPyStatement :: TTermDefinition (Py.SimpleStatement -> Py.Statement)
pySimpleStatementToPyStatement = def "pySimpleStatementToPyStatement" $
  doc "Convert a SimpleStatement to a Statement" $
  lambda "s" $
    PyDsl.statementSimple $ list [var "s"]

-- | Convert an Expression to a SimpleStatement
pyExpressionToPySimpleStatement :: TTermDefinition (Py.Expression -> Py.SimpleStatement)
pyExpressionToPySimpleStatement = def "pyExpressionToPySimpleStatement" $
  doc "Convert an Expression to a SimpleStatement (as star expressions)" $
  lambda "expr" $
    PyDsl.simpleStatementStarExpressions $ list [PyDsl.starExpressionSimple $ var "expr"]

-- | Convert an Expression to a Statement
pyExpressionToPyStatement :: TTermDefinition (Py.Expression -> Py.Statement)
pyExpressionToPyStatement = def "pyExpressionToPyStatement" $
  doc "Convert an Expression to a Statement" $
  lambda "expr" $
    pySimpleStatementToPyStatement @@ (pyExpressionToPySimpleStatement @@ var "expr")

-- | Convert an Expression to an AnnotatedRhs
pyExpressionToPyAnnotatedRhs :: TTermDefinition (Py.Expression -> Py.AnnotatedRhs)
pyExpressionToPyAnnotatedRhs = def "pyExpressionToPyAnnotatedRhs" $
  doc "Convert an Expression to an AnnotatedRhs" $
  lambda "expr" $
    PyDsl.annotatedRhsStar $ list [PyDsl.starExpressionSimple $ var "expr"]

-- | Convert an Expression to a Slice
pyExpressionToPySlice :: TTermDefinition (Py.Expression -> Py.Slice)
pyExpressionToPySlice = def "pyExpressionToPySlice" $
  doc "Convert an Expression to a Slice" $
  lambda "expr" $
    PyDsl.sliceNamed $ PyDsl.namedExpressionSimple $ var "expr"

-- | Convert an Expression to a StarNamedExpression
pyExpressionToPyStarNamedExpression :: TTermDefinition (Py.Expression -> Py.StarNamedExpression)
pyExpressionToPyStarNamedExpression = def "pyExpressionToPyStarNamedExpression" $
  doc "Convert an Expression to a StarNamedExpression" $
  lambda "expr" $
    PyDsl.starNamedExpressionSimple $ PyDsl.namedExpressionSimple $ var "expr"

-- | Convert a list of Expressions to Args
pyExpressionsToPyArgs :: TTermDefinition ([Py.Expression] -> Py.Args)
pyExpressionsToPyArgs = def "pyExpressionsToPyArgs" $
  doc "Convert a list of Expressions to Args" $
  "exprs" ~>
    PyDsl.argsPositionalOnly $
      Lists.map ("e" ~> PyDsl.posArgExpression $ var "e") (var "exprs")

-- | Convert a Name to a StarTarget
pyNameToPyStarTarget :: TTermDefinition (Py.Name -> Py.StarTarget)
pyNameToPyStarTarget = def "pyNameToPyStarTarget" $
  doc "Convert a Name to a StarTarget" $
  lambda "name" $
    PyDsl.starTargetUnstarred $ PyDsl.targetWithStarAtomAtom $ PyDsl.starAtomName $ var "name"

-- | Convert a Name to a TypeParameter
pyNameToPyTypeParameter :: TTermDefinition (Py.Name -> Py.TypeParameter)
pyNameToPyTypeParameter = def "pyNameToPyTypeParameter" $
  doc "Convert a Name to a TypeParameter" $
  "name" ~>
    PyDsl.typeParameterSimple $ PyDsl.simpleTypeParameterSimple $ var "name"

-- | Convert a Name to a NamedExpression
pyNameToPyNamedExpression :: TTermDefinition (Py.Name -> Py.NamedExpression)
pyNameToPyNamedExpression = def "pyNameToPyNamedExpression" $
  doc "Convert a Name to a NamedExpression" $
  lambda "name" $
    PyDsl.namedExpressionSimple $ pyNameToPyExpression @@ var "name"

-- | Convert an Assignment to a Statement
pyAssignmentToPyStatement :: TTermDefinition (Py.Assignment -> Py.Statement)
pyAssignmentToPyStatement = def "pyAssignmentToPyStatement" $
  doc "Convert an Assignment to a Statement" $
  lambda "a" $
    pySimpleStatementToPyStatement @@ (PyDsl.simpleStatementAssignment $ var "a")

-- | Convert a ClassDefinition to a Statement
pyClassDefinitionToPyStatement :: TTermDefinition (Py.ClassDefinition -> Py.Statement)
pyClassDefinitionToPyStatement = def "pyClassDefinitionToPyStatement" $
  doc "Convert a ClassDefinition to a Statement" $
  lambda "cd" $
    PyDsl.statementCompound $ PyDsl.compoundStatementClassDef $ var "cd"

-- | Convert a ClosedPattern to Patterns
pyClosedPatternToPyPatterns :: TTermDefinition (Py.ClosedPattern -> Py.Patterns)
pyClosedPatternToPyPatterns = def "pyClosedPatternToPyPatterns" $
  doc "Convert a ClosedPattern to Patterns" $
  "p" ~>
    PyDsl.patternsPattern $ PyDsl.patternOr $ PyDsl.orPattern $ list [var "p"]

-- | Combine a Primary with a PrimaryRhs
primaryWithRhs :: TTermDefinition (Py.Primary -> Py.PrimaryRhs -> Py.Primary)
primaryWithRhs = def "primaryWithRhs" $
  doc "Combine a Primary with a PrimaryRhs" $
  "prim" ~> "rhs" ~>
    PyDsl.primaryCompound $ PyDsl.primaryWithRhs (var "prim") (var "rhs")

-- | Create a Primary with slices
primaryWithSlices :: TTermDefinition (Py.Primary -> Py.Slice -> [Py.SliceOrStarredExpression] -> Py.Primary)
primaryWithSlices = def "primaryWithSlices" $
  doc "Create a Primary with slices" $
  "prim" ~> "first" ~> "rest" ~>
    primaryWithRhs @@ var "prim" @@
      (PyDsl.primaryRhsSlices $
        PyDsl.slices (var "first") (var "rest"))

-- | Create a Primary with expression slices
primaryWithExpressionSlices :: TTermDefinition (Py.Primary -> [Py.Expression] -> Py.Primary)
primaryWithExpressionSlices = def "primaryWithExpressionSlices" $
  doc "Create a Primary with expression slices" $
  lambdas ["prim", "exprs"] $
    primaryWithSlices @@ var "prim"
      @@ (pyExpressionToPySlice @@ (Lists.head $ var "exprs"))
      @@ (Lists.map
            (lambda "e" $ PyDsl.sliceOrStarredExpressionSlice $ pyExpressionToPySlice @@ var "e")
            (Lists.tail $ var "exprs"))

-- | Create a function call expression
functionCall :: TTermDefinition (Py.Primary -> [Py.Expression] -> Py.Expression)
functionCall = def "functionCall" $
  doc "Create a function call expression" $
  lambdas ["func", "args"] $
    pyPrimaryToPyExpression @@
      (primaryWithRhs @@ var "func" @@
        (PyDsl.primaryRhsCall $ pyExpressionsToPyArgs @@ var "args"))

-- | Create a primary with parameters (subscript)
primaryAndParams :: TTermDefinition (Py.Primary -> [Py.Expression] -> Py.Expression)
primaryAndParams = def "primaryAndParams" $
  doc "Create a primary with parameters (subscript)" $
  lambdas ["prim", "params"] $
    pyPrimaryToPyExpression @@ (primaryWithExpressionSlices @@ var "prim" @@ var "params")

-- | Create a name with parameters
nameAndParams :: TTermDefinition (Py.Name -> [Py.Expression] -> Py.Expression)
nameAndParams = def "nameAndParams" $
  doc "Create a name with parameters" $
  lambdas ["pyName", "params"] $
    primaryAndParams @@ (pyNameToPyPrimary @@ var "pyName") @@ var "params"

-- | Create a string expression with a given quote style
stringToPyExpression :: TTermDefinition (Py.QuoteStyle -> String -> Py.Expression)
stringToPyExpression = def "stringToPyExpression" $
  doc "Create a string expression with a given quote style" $
  lambdas ["style", "s"] $
    pyAtomToPyExpression @@ (PyDsl.atomString $ PyDsl.string_ (var "s") (var "style"))

-- | Create a single-quoted string expression
singleQuotedString :: TTermDefinition (String -> Py.Expression)
singleQuotedString = def "singleQuotedString" $
  doc "Create a single-quoted string expression" $
  lambda "s" $
    stringToPyExpression @@ PyDsl.quoteStyleSingle @@ var "s"

-- | Create a double-quoted string expression
doubleQuotedString :: TTermDefinition (String -> Py.Expression)
doubleQuotedString = def "doubleQuotedString" $
  doc "Create a double-quoted string expression" $
  lambda "s" $
    stringToPyExpression @@ PyDsl.quoteStyleDouble @@ var "s"

-- | Create a triple-quoted string expression
tripleQuotedString :: TTermDefinition (String -> Py.Expression)
tripleQuotedString = def "tripleQuotedString" $
  doc "Create a triple-quoted string expression" $
  lambda "s" $
    stringToPyExpression @@ PyDsl.quoteStyleTriple @@ var "s"

-- | Create an assignment statement from name and annotated rhs
assignment :: TTermDefinition (Py.Name -> Py.AnnotatedRhs -> Py.Statement)
assignment = def "assignment" $
  doc "Create an assignment statement from name and annotated rhs" $
  "name" ~> "rhs" ~>
    pyAssignmentToPyStatement @@
      (PyDsl.assignmentUntyped $
        PyDsl.untypedAssignmentSimple
          (list [pyNameToPyStarTarget @@ var "name"])
          (var "rhs"))

-- | Create an assignment statement from name and expression
assignmentStatement :: TTermDefinition (Py.Name -> Py.Expression -> Py.Statement)
assignmentStatement = def "assignmentStatement" $
  doc "Create an assignment statement from name and expression" $
  lambdas ["name", "expr"] $
    assignment @@ var "name" @@ (pyExpressionToPyAnnotatedRhs @@ var "expr")

-- | Create a dotted assignment statement: obj.attr = expr
dottedAssignmentStatement :: TTermDefinition (Py.Name -> Py.Name -> Py.Expression -> Py.Statement)
dottedAssignmentStatement = def "dottedAssignmentStatement" $
  doc "Create a dotted assignment statement: obj.attr = expr" $
  lambdas ["obj", "attr", "expr"] $
    "target" <~ (PyDsl.starTargetUnstarred $
      inject Py._TargetWithStarAtom Py._TargetWithStarAtom_project $
        record Py._TPrimaryAndName [
          Py._TPrimaryAndName_primary>>: inject Py._TPrimary Py._TPrimary_atom $
            inject Py._Atom Py._Atom_name $ var "obj",
          Py._TPrimaryAndName_name>>: var "attr"]) $
    pyAssignmentToPyStatement @@
      (PyDsl.assignmentUntyped $
        PyDsl.untypedAssignmentSimple
          (list [var "target"])
          (pyExpressionToPyAnnotatedRhs @@ var "expr"))

-- | Create a return statement with a single expression
returnSingle :: TTermDefinition (Py.Expression -> Py.Statement)
returnSingle = def "returnSingle" $
  doc "Create a return statement with a single expression" $
  "expr" ~>
    pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementReturn $
        PyDsl.returnStatement $ list [PyDsl.starExpressionSimple $ var "expr"])

-- | Create a cast expression
castTo :: TTermDefinition (Py.Expression -> Py.Expression -> Py.Expression)
castTo = def "castTo" $
  doc "Create a cast expression: cast(type, expr)" $
  "pytype" ~> "pyexpr" ~>
    functionCall @@ (pyNameToPyPrimary @@ (PyDsl.name $ string "cast")) @@ list [var "pytype", var "pyexpr"]

-- | Project a field from an expression
projectFromExpression :: TTermDefinition (Py.Expression -> Py.Name -> Py.Expression)
projectFromExpression = def "projectFromExpression" $
  doc "Project a field from an expression" $
  "exp" ~> "name" ~>
    "prim" <~ (PyDsl.primarySimple $ PyDsl.atomGroup $
        PyDsl.groupExpression $ PyDsl.namedExpressionSimple $ var "exp") $
    pyPrimaryToPyExpression @@
      (PyDsl.primaryCompound $ PyDsl.primaryWithRhs (var "prim") (PyDsl.primaryRhsProject $ var "name"))

-- | Annotate a statement with an optional comment
annotatedStatement :: TTermDefinition (Maybe String -> Py.Statement -> Py.Statement)
annotatedStatement = def "annotatedStatement" $
  doc "Annotate a statement with an optional comment" $
  lambdas ["mcomment", "stmt"] $
    Maybes.maybe (var "stmt")
      (lambda "c" $ PyDsl.statementAnnotated $ PyDsl.annotatedStatement (var "c") (var "stmt"))
      (var "mcomment")

-- | Annotate an expression with an optional comment using Annotated[]
annotatedExpression :: TTermDefinition (Maybe String -> Py.Expression -> Py.Expression)
annotatedExpression = def "annotatedExpression" $
  doc "Annotate an expression with an optional comment using Annotated[]" $
  "mcomment" ~> "expr" ~>
    Maybes.maybe (var "expr")
      ("c" ~>
        pyPrimaryToPyExpression @@
          (primaryWithExpressionSlices
            @@ (pyNameToPyPrimary @@ (PyDsl.name $ string "Annotated"))
            @@ list [var "expr", doubleQuotedString @@ var "c"]))
      (var "mcomment")

-- | Create a comment statement (triple-quoted string)
commentStatement :: TTermDefinition (String -> Py.Statement)
commentStatement = def "commentStatement" $
  doc "Create a comment statement (triple-quoted string)" $
  lambda "s" $
    pyExpressionToPyStatement @@ (tripleQuotedString @@ var "s")

-- | Create a raise AssertionError statement
raiseAssertionError :: TTermDefinition (String -> Py.Statement)
raiseAssertionError = def "raiseAssertionError" $
  doc "Create a raise AssertionError statement" $
  "msg" ~>
    pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementRaise $ PyDsl.raiseStatement $ just $
        PyDsl.raiseExpression
          (functionCall @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "AssertionError")
            @@ list [doubleQuotedString @@ var "msg"])
          nothing)

-- | Create a raise TypeError statement
raiseTypeError :: TTermDefinition (String -> Py.Statement)
raiseTypeError = def "raiseTypeError" $
  doc "Create a raise TypeError statement" $
  "msg" ~>
    pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementRaise $ PyDsl.raiseStatement $ just $
        PyDsl.raiseExpression
          (functionCall @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "TypeError")
            @@ list [doubleQuotedString @@ var "msg"])
          nothing)

-- | Create a NewType statement
newtypeStatement :: TTermDefinition (Py.Name -> Maybe String -> Py.Expression -> Py.Statement)
newtypeStatement = def "newtypeStatement" $
  doc "Create a NewType statement" $
  lambdas ["name", "mcomment", "expr"] $
    annotatedStatement @@ var "mcomment" @@
      (assignmentStatement @@ var "name" @@
        (functionCall @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "NewType")
          @@ list [doubleQuotedString @@ (PyDsl.unName $ var "name"), var "expr"]))

-- | Generate a type alias statement using PEP 695 syntax (Python 3.12+)
typeAliasStatement :: TTermDefinition (Py.Name -> [Py.TypeParameter] -> Maybe String -> Py.Expression -> Py.Statement)
typeAliasStatement = def "typeAliasStatement" $
  doc "Generate a type alias statement using PEP 695 syntax (Python 3.12+)" $
  "name" ~> "tparams" ~> "mcomment" ~> "tyexpr" ~>
    annotatedStatement @@ var "mcomment" @@
      (pySimpleStatementToPyStatement @@
        (PyDsl.simpleStatementTypeAlias $
          PyDsl.typeAlias (var "name") (var "tparams") (var "tyexpr")))

-- | Create a Python list from expressions
pyList :: TTermDefinition ([Py.Expression] -> Py.List)
pyList = def "pyList" $
  doc "Create a Python list from expressions" $
  "exprs" ~>
    PyDsl.list_ $ Lists.map pyExpressionToPyStarNamedExpression (var "exprs")

-- | Decode a Power to a Primary if possible
decodePyPowerToPyPrimary :: TTermDefinition (Py.Power -> Maybe Py.Primary)
decodePyPowerToPyPrimary = def "decodePyPowerToPyPrimary" $
  doc "Decode a Power to a Primary if possible" $
  lambda "p" $ lets [
    "lhs">: PyDsl.powerLhs $ var "p",
    "await">: PyDsl.awaitPrimaryAwait $ var "lhs",
    "prim">: PyDsl.awaitPrimaryPrimary $ var "lhs"] $
    Logic.ifElse (var "await")
      nothing
      (just $ var "prim")

-- | Decode a Comparison to a Primary if possible
decodePyComparisonToPyAwaitPrimary :: TTermDefinition (Py.Comparison -> Maybe Py.Primary)
decodePyComparisonToPyAwaitPrimary = def "decodePyComparisonToPyAwaitPrimary" $
  doc "Decode a Comparison to a Primary if possible" $
  lambda "c" $ lets [
    "rhs">: PyDsl.comparisonRhs $ var "c",
    "lhs">: PyDsl.comparisonLhs $ var "c",
    "orLhs">: PyDsl.bitwiseOrLhs $ var "lhs",
    "orRhs">: PyDsl.bitwiseOrRhs $ var "lhs",
    "xorLhs">: PyDsl.bitwiseXorLhs $ var "orRhs",
    "xorRhs">: PyDsl.bitwiseXorRhs $ var "orRhs",
    "andLhs">: PyDsl.bitwiseAndLhs $ var "xorRhs",
    "andRhs">: PyDsl.bitwiseAndRhs $ var "xorRhs",
    "shiftLhs">: project Py._ShiftExpression Py._ShiftExpression_lhs @@ var "andRhs",
    "shiftRhs">: PyDsl.shiftExpressionRhs $ var "andRhs",
    "sumLhs">: project Py._Sum Py._Sum_lhs @@ var "shiftRhs",
    "sumRhs">: PyDsl.sumRhs $ var "shiftRhs",
    "termLhs">: project Py._Term Py._Term_lhs @@ var "sumRhs",
    "termRhs">: PyDsl.termRhs $ var "sumRhs"] $
    -- Check if any intermediate optional fields are set (which would indicate non-simple structure)
    Logic.ifElse (Logic.not $ Lists.null $ var "rhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "orLhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "xorLhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "andLhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "shiftLhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "sumLhs") nothing $
    Logic.ifElse (Maybes.isJust $ var "termLhs") nothing $
    -- Now match on termRhs to see if it's a simple factor
    (match Py._Factor (Just nothing) [
      Py._Factor_simple>>: lambda "power" $ decodePyPowerToPyPrimary @@ var "power"]
    @@ var "termRhs")

-- | Decode an Inversion to a Primary if possible
decodePyInversionToPyPrimary :: TTermDefinition (Py.Inversion -> Maybe Py.Primary)
decodePyInversionToPyPrimary = def "decodePyInversionToPyPrimary" $
  doc "Decode an Inversion to a Primary if possible" $
  lambda "i" $
    (match Py._Inversion (Just nothing) [
      Py._Inversion_simple>>: lambda "comparison" $
        decodePyComparisonToPyAwaitPrimary @@ var "comparison"]
    @@ var "i")

-- | Decode a Conjunction to a Primary if possible
decodePyConjunctionToPyPrimary :: TTermDefinition (Py.Conjunction -> Maybe Py.Primary)
decodePyConjunctionToPyPrimary = def "decodePyConjunctionToPyPrimary" $
  doc "Decode a Conjunction to a Primary if possible" $
  lambda "c" $ lets [
    "inversions">: PyDsl.unConjunction $ var "c"] $
    Logic.ifElse (Equality.equal (Lists.length $ var "inversions") (int32 1))
      (decodePyInversionToPyPrimary @@ (Lists.head $ var "inversions"))
      nothing

-- | Decode an Expression to a Primary if possible
decodePyExpressionToPyPrimary :: TTermDefinition (Py.Expression -> Maybe Py.Primary)
decodePyExpressionToPyPrimary = def "decodePyExpressionToPyPrimary" $
  doc "Decode an Expression to a Primary if possible" $
  lambda "e" $
    (match Py._Expression (Just nothing) [
      Py._Expression_simple>>: lambda "disj" $ lets [
        "conjunctions">: PyDsl.unDisjunction $ var "disj"] $
        Logic.ifElse (Equality.equal (Lists.length $ var "conjunctions") (int32 1))
          (decodePyConjunctionToPyPrimary @@ (Lists.head $ var "conjunctions"))
          nothing]
    @@ var "e")

-- | Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary
pyExpressionToPyPrimary :: TTermDefinition (Py.Expression -> Py.Primary)
pyExpressionToPyPrimary = def "pyExpressionToPyPrimary" $
  doc "Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary" $
  lambda "e" $
    Maybes.maybe
      (PyDsl.primarySimple $ PyDsl.atomGroup $ PyDsl.groupExpression $
        PyDsl.namedExpressionSimple $ var "e")
      (lambda "prim" $ var "prim")
      (decodePyExpressionToPyPrimary @@ var "e")

-- | Convert an Expression to a Disjunction.
--   For ExpressionSimple(disj), extracts the disjunction.
--   For other expressions (Conditional, Lambda), wraps in parentheses.
pyExpressionToDisjunction :: TTermDefinition (Py.Expression -> Py.Disjunction)
pyExpressionToDisjunction = def "pyExpressionToDisjunction" $
  doc "Convert an Expression to a Disjunction, wrapping in parens if needed" $
  lambda "e" $
    cases Py._Expression (var "e")
      -- Default for non-simple expressions: wrap in parens to get a primary, then to disjunction
      (Just $ PyDsl.disjunction $ list [
        pyPrimaryToPyConjunction @@
          (PyDsl.primarySimple $ PyDsl.atomGroup $ PyDsl.groupExpression $
            PyDsl.namedExpressionSimple $ var "e")]) [
      -- Simple expressions already contain a disjunction
      Py._Expression_simple>>: "disj" ~> var "disj"]

-- | Convert an Expression to a BitwiseOr, wrapping in parens if needed
pyExpressionToBitwiseOr :: TTermDefinition (Py.Expression -> Py.BitwiseOr)
pyExpressionToBitwiseOr = def "pyExpressionToBitwiseOr" $
  doc "Convert an Expression to a BitwiseOr, wrapping in parens if needed" $
  lambda "e" $
    PyDsl.pyPrimaryToPyBitwiseOr
      (PyDsl.primarySimple $ PyDsl.atomGroup $ PyDsl.groupExpression $
        PyDsl.namedExpressionSimple $ var "e")

-- | Convert a Primary to a Slice
pyPrimaryToPySlice :: TTermDefinition (Py.Primary -> Py.Slice)
pyPrimaryToPySlice = def "pyPrimaryToPySlice" $
  doc "Convert a Primary to a Slice" $
  lambda "prim" $
    pyExpressionToPySlice @@ (pyPrimaryToPyExpression @@ var "prim")

-- | Convert a BitwiseOr to an Expression
pyBitwiseOrToPyExpression :: TTermDefinition (Py.BitwiseOr -> Py.Expression)
pyBitwiseOrToPyExpression = def "pyBitwiseOrToPyExpression" $
  doc "Convert a BitwiseOr to an Expression" $
  lambda "bor" $
    pyConjunctionToPyExpression @@ (pyBitwiseOrToPyConjunction @@ var "bor")

-- | Create an indented block with optional comment
indentedBlock :: TTermDefinition (Maybe String -> [[Py.Statement]] -> Py.Block)
indentedBlock = def "indentedBlock" $
  doc "Create an indented block with optional comment" $
  lambdas ["mcomment", "stmts"] $ lets [
    "commentGroup">: Maybes.maybe
      (list ([] :: [TTerm Py.Statement]))
      (lambda "s" $ list [commentStatement @@ var "s"])
      (var "mcomment"),
    "groups">: Lists.filter (lambda "g" $ Logic.not $ Lists.null $ var "g")
      (Lists.cons (var "commentGroup") (var "stmts"))] $
    Logic.ifElse (Lists.null $ var "groups")
      (PyDsl.blockIndented $ list [list [
        PyDsl.statementSimple $ list [
          pyExpressionToPySimpleStatement @@ (pyAtomToPyExpression @@ PyDsl.atomEllipsis)]]])
      (PyDsl.blockIndented $ var "groups")

-- | Build an or-expression from multiple primaries
orExpression :: TTermDefinition ([Py.Primary] -> Py.Expression)
orExpression = def "orExpression" $
  doc "Build an or-expression from multiple primaries" $
  "prims" ~>
    "build" <~ ("prev" ~> "ps" ~>
      Logic.ifElse (Lists.null $ Lists.tail $ var "ps")
        (PyDsl.bitwiseOr (var "prev") (pyPrimaryToPyBitwiseXor @@ (Lists.head $ var "ps")))
        (var "build"
          @@ (just $ PyDsl.bitwiseOr (var "prev") (pyPrimaryToPyBitwiseXor @@ (Lists.head $ var "ps")))
          @@ (Lists.tail $ var "ps"))) $
    pyBitwiseOrToPyExpression @@ (var "build" @@ nothing @@ var "prims")

-- | Generate a type alias statement using Python 3.10-compatible syntax
typeAliasStatement310 :: TTermDefinition (Py.Name -> [Py.TypeParameter] -> Maybe String -> Py.Expression -> Py.Statement)
typeAliasStatement310 = def "typeAliasStatement310" $
  doc "Generate a type alias statement using Python 3.10-compatible syntax: Name: TypeAlias = \"TypeExpression\"" $
  "name" ~> "_tparams" ~> "mcomment" ~> "tyexpr" ~>
    "quotedExpr" <~ (doubleQuotedString @@ (Serialization.printExpr @@ (PySerde.encodeExpression @@ var "tyexpr"))) $
    annotatedStatement @@ var "mcomment" @@
      (pyAssignmentToPyStatement @@
        (PyDsl.assignmentTyped $
          PyDsl.typedAssignment
            (PyDsl.singleTargetName $ var "name")
            (PyDsl.pyNameToPyExpression $ PyDsl.name $ string "TypeAlias")
            (just $ pyExpressionToPyAnnotatedRhs @@ var "quotedExpr")))

-- | Generate __getitem__ method parameters for metaclass
getItemParams :: TTermDefinition Py.Parameters
getItemParams = def "getItemParams" $
  PyDsl.parametersParamNoDefault $
    PyDsl.paramNoDefaultParametersSimple $ list [
      PyDsl.paramNoDefaultSimple $ PyDsl.paramSimple $ PyDsl.name $ string "cls",
      PyDsl.paramNoDefaultSimple $ PyDsl.paramSimple $ PyDsl.name $ string "item"]

-- | Generate a subscriptable union class for Python 3.10
unionTypeClassStatements310 :: TTermDefinition (Py.Name -> Maybe String -> Py.Expression -> [Py.Statement] -> [Py.Statement])
unionTypeClassStatements310 = def "unionTypeClassStatements310" $
  doc "Generate a subscriptable union class for Python 3.10" $
  "name" ~> "mcomment" ~> "tyexpr" ~> "extraStmts" ~>
    "nameStr" <~ (PyDsl.unName $ var "name") $
    "metaName" <~ (PyDsl.name $ string "_" ++ var "nameStr" ++ string "Meta") $
    "docString" <~ (Serialization.printExpr @@ (PySerde.encodeExpression @@ var "tyexpr")) $
    -- return object statement
    "returnObject" <~ (pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementReturn $
        PyDsl.returnStatement $ list [
          PyDsl.starExpressionSimple $
            PyDsl.pyNameToPyExpression $ PyDsl.name $ string "object"])) $
    -- def __getitem__(cls, item): return object
    "getItemMethod" <~ (PyDsl.statementCompound $
      PyDsl.compoundStatementFunction $
        PyDsl.functionDefinition nothing $
          PyDsl.functionDefRaw false
            (PyDsl.name $ string "__getitem__")
            (list ([] :: [TTerm Py.TypeParameter]))
            (just getItemParams)
            nothing
            nothing
            (indentedBlock @@ nothing @@ list [list [var "returnObject"]])) $
    -- class _NameMeta(type): ...
    "metaClass" <~ (pyClassDefinitionToPyStatement @@
      PyDsl.classDefinition nothing (var "metaName")
        (list ([] :: [TTerm Py.TypeParameter]))
        (just $ pyExpressionsToPyArgs @@ list [PyDsl.pyNameToPyExpression $ PyDsl.name $ string "type"])
        (indentedBlock @@ nothing @@ list [list [var "getItemMethod"]])) $
    -- docstring statement
    "docStmt" <~ (pyExpressionToPyStatement @@ (tripleQuotedString @@ var "docString")) $
    -- Build body groups: docstring, then extra statements (constants), then pass if no extras
    "bodyGroups" <~ Logic.ifElse (Lists.null $ var "extraStmts")
      ("passStmt" <~ (pySimpleStatementToPyStatement @@ PyDsl.simpleStatementPass) $
       list [list [var "docStmt"], list [var "passStmt"]])
      (list [list [var "docStmt"], var "extraStmts"]) $
    -- metaclass kwarg
    "metaclassArg" <~ (PyDsl.kwarg (PyDsl.name $ string "metaclass") (PyDsl.pyNameToPyExpression $ var "metaName")) $
    -- class Name(metaclass=_NameMeta): ...
    "unionClass" <~ (annotatedStatement @@ var "mcomment" @@
      (pyClassDefinitionToPyStatement @@
        PyDsl.classDefinition nothing (var "name")
          (list ([] :: [TTerm Py.TypeParameter]))
          (just $ PyDsl.args
            (list ([] :: [TTerm Py.PosArg]))
            (list [PyDsl.kwargOrStarredKwarg $ var "metaclassArg"])
            (list ([] :: [TTerm Py.KwargOrDoubleStarred])))
          (indentedBlock @@ nothing @@ var "bodyGroups"))) $
    list [var "metaClass", var "unionClass"]

-- | Generate __eq__ and __hash__ method parameters
selfOnlyParams :: TTermDefinition Py.Parameters
selfOnlyParams = def "selfOnlyParams" $
  PyDsl.parametersParamNoDefault $
    PyDsl.paramNoDefaultParametersSimple $ list [
      PyDsl.paramNoDefaultSimple $ PyDsl.paramSimple $ PyDsl.name $ string "self"]

-- | Generate self and other parameters
selfOtherParams :: TTermDefinition Py.Parameters
selfOtherParams = def "selfOtherParams" $
  PyDsl.parametersParamNoDefault $
    PyDsl.paramNoDefaultParametersSimple $ list [
      PyDsl.paramNoDefaultSimple $ PyDsl.paramSimple $ PyDsl.name $ string "self",
      PyDsl.paramNoDefaultSimple $ PyDsl.paramSimple $ PyDsl.name $ string "other"]

-- | Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants
unitVariantMethods :: TTermDefinition (Py.Name -> [Py.Statement])
unitVariantMethods = def "unitVariantMethods" $
  doc "Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants" $
  "className" ~>
    "classNameStr" <~ (PyDsl.unName $ var "className") $
    -- __slots__ = ()
    "slotsStmt" <~ (assignmentStatement @@ (PyDsl.name $ string "__slots__") @@
      (pyPrimaryToPyExpression @@ (PyDsl.primarySimple $
        PyDsl.atomTuple $ PyDsl.tuple $ list ([] :: [TTerm Py.StarNamedExpression])))) $
    -- return isinstance(other, ClassName)
    "returnIsinstance" <~ (pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementReturn $
        PyDsl.returnStatement $ list [
          PyDsl.starExpressionSimple $
            functionCall @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "isinstance")
              @@ list [PyDsl.pyNameToPyExpression $ PyDsl.name $ string "other",
                       PyDsl.pyNameToPyExpression $ var "className"]])) $
    -- def __eq__(self, other): return isinstance(other, ClassName)
    "eqMethod" <~ (PyDsl.statementCompound $
      PyDsl.compoundStatementFunction $
        PyDsl.functionDefinition nothing $
          PyDsl.functionDefRaw false
            (PyDsl.name $ string "__eq__")
            (list ([] :: [TTerm Py.TypeParameter]))
            (just selfOtherParams)
            nothing
            nothing
            (indentedBlock @@ nothing @@ list [list [var "returnIsinstance"]])) $
    -- return hash("ClassName")
    "returnHash" <~ (pySimpleStatementToPyStatement @@
      (PyDsl.simpleStatementReturn $
        PyDsl.returnStatement $ list [
          PyDsl.starExpressionSimple $
            functionCall @@ (PyDsl.pyNameToPyPrimary $ PyDsl.name $ string "hash")
              @@ list [doubleQuotedString @@ var "classNameStr"]])) $
    -- def __hash__(self): return hash("ClassName")
    "hashMethod" <~ (PyDsl.statementCompound $
      PyDsl.compoundStatementFunction $
        PyDsl.functionDefinition nothing $
          PyDsl.functionDefRaw false
            (PyDsl.name $ string "__hash__")
            (list ([] :: [TTerm Py.TypeParameter]))
            (just selfOnlyParams)
            nothing
            nothing
            (indentedBlock @@ nothing @@ list [list [var "returnHash"]])) $
    list [var "slotsStmt", var "eqMethod", var "hashMethod"]

-- | Find all namespaces referenced by a list of definitions, plus the core namespace
findNamespaces :: TTermDefinition (Namespace -> [Definition] -> Namespaces Py.DottedName)
findNamespaces = def "findNamespaces" $
  doc "Find all namespaces referenced by a list of definitions, plus the core namespace" $
  lambdas ["focusNs", "defs"] $ lets [
    "coreNs">: Packaging.namespace $ string "hydra.core",
    "namespaces">: Analysis.namespacesForDefinitions @@ PyNames.encodeNamespace @@ var "focusNs" @@ var "defs"] $
    Logic.ifElse (Equality.equal
      (Packaging.unNamespace $ Pairs.first $ Packaging.namespacesFocus $ var "namespaces")
      (Packaging.unNamespace $ var "coreNs"))
      (var "namespaces")
      (Packaging.namespaces
        (Packaging.namespacesFocus $ var "namespaces")
        (Maps.insert (var "coreNs")
          (PyNames.encodeNamespace @@ var "coreNs")
          (Packaging.namespacesMapping $ var "namespaces")))
