-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.python.utils

module Hydra.Dsl.Python.Utils where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Analysis as DslAnalysis
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Python.Environment as PythonEnvironment
import qualified Hydra.Dsl.Python.Names as PythonNames
import qualified Hydra.Dsl.Python.Syntax as PythonSyntax
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Serialization as DslSerialization
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Python.Environment as Environment
import qualified Hydra.Python.Names as Names
import qualified Hydra.Python.Serde as Serde
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Python.Utils as Utils
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL reference to hydra.python.utils.annotatedExpression
annotatedExpression :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression
annotatedExpression arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.annotatedExpression")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.annotatedStatement
annotatedStatement :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.Statement
annotatedStatement arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.annotatedStatement")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.assignment
assignment :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.AnnotatedRhs -> Typed.TypedTerm Syntax.Statement
assignment arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.assignment")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.assignmentStatement
assignmentStatement :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
assignmentStatement arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.assignmentStatement")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.castTo
castTo :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression
castTo arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.castTo")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.commentStatement
commentStatement :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Statement
commentStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.commentStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.decodePyComparisonToPyAwaitPrimary
decodePyComparisonToPyAwaitPrimary :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm (Maybe Syntax.Primary)
decodePyComparisonToPyAwaitPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.decodePyComparisonToPyAwaitPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.decodePyConjunctionToPyPrimary
decodePyConjunctionToPyPrimary :: Typed.TypedTerm Syntax.Conjunction -> Typed.TypedTerm (Maybe Syntax.Primary)
decodePyConjunctionToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.decodePyConjunctionToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.decodePyExpressionToPyPrimary
decodePyExpressionToPyPrimary :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Primary)
decodePyExpressionToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.decodePyExpressionToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.decodePyInversionToPyPrimary
decodePyInversionToPyPrimary :: Typed.TypedTerm Syntax.Inversion -> Typed.TypedTerm (Maybe Syntax.Primary)
decodePyInversionToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.decodePyInversionToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.decodePyPowerToPyPrimary
decodePyPowerToPyPrimary :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm (Maybe Syntax.Primary)
decodePyPowerToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.decodePyPowerToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.dottedAssignmentStatement
dottedAssignmentStatement :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
dottedAssignmentStatement arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.dottedAssignmentStatement")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.python.utils.doubleQuotedString
doubleQuotedString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Expression
doubleQuotedString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.doubleQuotedString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.findNamespaces
findNamespaces :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm (Util.ModuleNames Syntax.DottedName)
findNamespaces arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.findNamespaces")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.functionCall
functionCall :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
functionCall arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.functionCall")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.getItemParams
getItemParams :: Typed.TypedTerm Syntax.Parameters
getItemParams = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.utils.getItemParams"))

-- | DSL reference to hydra.python.utils.indentedBlock
indentedBlock :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm [[Syntax.Statement]] -> Typed.TypedTerm Syntax.Block
indentedBlock arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.indentedBlock")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.nameAndParams
nameAndParams :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
nameAndParams arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.nameAndParams")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.newtypeStatement
newtypeStatement :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
newtypeStatement arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.newtypeStatement")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.python.utils.orExpression
orExpression :: Typed.TypedTerm [Syntax.Primary] -> Typed.TypedTerm Syntax.Expression
orExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.orExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.primaryAndParams
primaryAndParams :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
primaryAndParams arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.primaryAndParams")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.primaryWithExpressionSlices
primaryWithExpressionSlices :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Primary
primaryWithExpressionSlices arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.primaryWithExpressionSlices")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.primaryWithRhs
primaryWithRhs :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.PrimaryRhs -> Typed.TypedTerm Syntax.Primary
primaryWithRhs arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.primaryWithRhs")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.primaryWithSlices
primaryWithSlices :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm [Syntax.SliceOrStarredExpression] -> Typed.TypedTerm Syntax.Primary
primaryWithSlices arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.primaryWithSlices")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.python.utils.projectFromExpression
projectFromExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression
projectFromExpression arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.projectFromExpression")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.pyAssignmentToPyStatement
pyAssignmentToPyStatement :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.Statement
pyAssignmentToPyStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyAssignmentToPyStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyAtomToPyExpression
pyAtomToPyExpression :: Typed.TypedTerm Syntax.Atom -> Typed.TypedTerm Syntax.Expression
pyAtomToPyExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyAtomToPyExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyBitwiseOrToPyConjunction
pyBitwiseOrToPyConjunction :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.Conjunction
pyBitwiseOrToPyConjunction arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyBitwiseOrToPyConjunction")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyBitwiseOrToPyExpression
pyBitwiseOrToPyExpression :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.Expression
pyBitwiseOrToPyExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyBitwiseOrToPyExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyClassDefinitionToPyStatement
pyClassDefinitionToPyStatement :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.Statement
pyClassDefinitionToPyStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyClassDefinitionToPyStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyClosedPatternToPyPatterns
pyClosedPatternToPyPatterns :: Typed.TypedTerm Syntax.ClosedPattern -> Typed.TypedTerm Syntax.Patterns
pyClosedPatternToPyPatterns arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyClosedPatternToPyPatterns")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyConjunctionToPyExpression
pyConjunctionToPyExpression :: Typed.TypedTerm Syntax.Conjunction -> Typed.TypedTerm Syntax.Expression
pyConjunctionToPyExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyConjunctionToPyExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToBitwiseOr
pyExpressionToBitwiseOr :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BitwiseOr
pyExpressionToBitwiseOr arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToBitwiseOr")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToDisjunction
pyExpressionToDisjunction :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Disjunction
pyExpressionToDisjunction arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToDisjunction")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPyAnnotatedRhs
pyExpressionToPyAnnotatedRhs :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AnnotatedRhs
pyExpressionToPyAnnotatedRhs arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPyAnnotatedRhs")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPyPrimary
pyExpressionToPyPrimary :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Primary
pyExpressionToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPySimpleStatement
pyExpressionToPySimpleStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SimpleStatement
pyExpressionToPySimpleStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPySimpleStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPySlice
pyExpressionToPySlice :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Slice
pyExpressionToPySlice arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPySlice")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPyStarNamedExpression
pyExpressionToPyStarNamedExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.StarNamedExpression
pyExpressionToPyStarNamedExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPyStarNamedExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionToPyStatement
pyExpressionToPyStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
pyExpressionToPyStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionToPyStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyExpressionsToPyArgs
pyExpressionsToPyArgs :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Args
pyExpressionsToPyArgs arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyExpressionsToPyArgs")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyList
pyList :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.List
pyList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNameToPyExpression
pyNameToPyExpression :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression
pyNameToPyExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyNameToPyExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNameToPyNamedExpression
pyNameToPyNamedExpression :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.NamedExpression
pyNameToPyNamedExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyNameToPyNamedExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNameToPyPrimary
pyNameToPyPrimary :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Primary
pyNameToPyPrimary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyNameToPyPrimary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNameToPyStarTarget
pyNameToPyStarTarget :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.StarTarget
pyNameToPyStarTarget arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyNameToPyStarTarget")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNameToPyTypeParameter
pyNameToPyTypeParameter :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TypeParameter
pyNameToPyTypeParameter arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyNameToPyTypeParameter")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyNone
pyNone :: Typed.TypedTerm Syntax.Name
pyNone = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.utils.pyNone"))

-- | DSL reference to hydra.python.utils.pyPrimaryToPyBitwiseOr
pyPrimaryToPyBitwiseOr :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.BitwiseOr
pyPrimaryToPyBitwiseOr arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyPrimaryToPyBitwiseOr")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyPrimaryToPyBitwiseXor
pyPrimaryToPyBitwiseXor :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.BitwiseXor
pyPrimaryToPyBitwiseXor arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyPrimaryToPyBitwiseXor")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyPrimaryToPyConjunction
pyPrimaryToPyConjunction :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.Conjunction
pyPrimaryToPyConjunction arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyPrimaryToPyConjunction")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyPrimaryToPyExpression
pyPrimaryToPyExpression :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.Expression
pyPrimaryToPyExpression arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyPrimaryToPyExpression")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pyPrimaryToPySlice
pyPrimaryToPySlice :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.Slice
pyPrimaryToPySlice arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pyPrimaryToPySlice")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.pySimpleStatementToPyStatement
pySimpleStatementToPyStatement :: Typed.TypedTerm Syntax.SimpleStatement -> Typed.TypedTerm Syntax.Statement
pySimpleStatementToPyStatement arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.pySimpleStatementToPyStatement")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.raiseAssertionError
raiseAssertionError :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Statement
raiseAssertionError arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.raiseAssertionError")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.raiseTypeError
raiseTypeError :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Statement
raiseTypeError arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.raiseTypeError")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.returnSingle
returnSingle :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
returnSingle arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.returnSingle")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.selfOnlyParams
selfOnlyParams :: Typed.TypedTerm Syntax.Parameters
selfOnlyParams = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.utils.selfOnlyParams"))

-- | DSL reference to hydra.python.utils.selfOtherParams
selfOtherParams :: Typed.TypedTerm Syntax.Parameters
selfOtherParams = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.utils.selfOtherParams"))

-- | DSL reference to hydra.python.utils.singleQuotedString
singleQuotedString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Expression
singleQuotedString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.singleQuotedString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.stringToPyExpression
stringToPyExpression :: Typed.TypedTerm Syntax.QuoteStyle -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.Expression
stringToPyExpression arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.stringToPyExpression")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.utils.targetPythonVersion
targetPythonVersion :: Typed.TypedTerm Environment.PythonVersion
targetPythonVersion = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.utils.targetPythonVersion"))

-- | DSL reference to hydra.python.utils.tripleQuotedString
tripleQuotedString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Expression
tripleQuotedString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.tripleQuotedString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.utils.typeAliasStatement
typeAliasStatement :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
typeAliasStatement arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.typeAliasStatement")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.python.utils.typeAliasStatement310
typeAliasStatement310 :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm t0 -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
typeAliasStatement310 arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.typeAliasStatement310")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.python.utils.unionTypeClassStatements310
unionTypeClassStatements310 :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm [Syntax.Statement]
unionTypeClassStatements310 arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.unionTypeClassStatements310")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.python.utils.unitVariantMethods
unitVariantMethods :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Statement]
unitVariantMethods arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.utils.unitVariantMethods")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
