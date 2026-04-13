-- | Convenience layer over the generated Python syntax DSL.
-- Re-exports all generated DSL functions and adds custom helpers
-- for common Python AST construction patterns.

module Hydra.Dsl.Python.Helpers (
  module Gen,
  module Hydra.Dsl.Python.Helpers,
) where

import Hydra.Kernel
import Hydra.Dsl.Python.Syntax as Gen hiding (list)
import Hydra.Dsl.Meta.Phantoms (nothing, just, false, list, record, inject, injectUnit, project, wrap, unwrap, (>>:), (@@))
import qualified Hydra.Python.Syntax as Py

import Prelude hiding (map)


-- Custom helpers

argsPositionalOnly :: TTerm [Py.PosArg] -> TTerm Py.Args
argsPositionalOnly pos = args pos
  (list ([] :: [TTerm Py.KwargOrStarred]))
  (list ([] :: [TTerm Py.KwargOrDoubleStarred]))

classPatternSimple :: TTerm Py.NameOrAttribute -> TTerm Py.ClassPattern
classPatternSimple nameOrAttr = classPattern nameOrAttr nothing nothing

classPatternWithKeywords :: TTerm Py.NameOrAttribute -> TTerm Py.KeywordPatterns -> TTerm Py.ClassPattern
classPatternWithKeywords nameOrAttr kwPatterns = classPattern nameOrAttr nothing (just kwPatterns)

compPairEq :: TTerm Py.BitwiseOr -> TTerm Py.CompareOpBitwiseOrPair
compPairEq rhs = compareOpBitwiseOrPair compareOpEq rhs

doubleQuotedString :: TTerm String -> TTerm Py.String_
doubleQuotedString val = string_ val quoteStyleDouble

functionDefRawSimple :: TTerm Py.Name -> TTerm (Maybe Py.Parameters) -> TTerm Py.Block -> TTerm Py.FunctionDefRaw
functionDefRawSimple name_ params block_ = functionDefRaw false name_ (list ([] :: [TTerm Py.TypeParameter])) params nothing nothing block_

functionDefinitionSimple :: TTerm Py.FunctionDefRaw -> TTerm Py.FunctionDefinition
functionDefinitionSimple raw = functionDefinition nothing raw

lambdaParametersEmpty :: TTerm Py.LambdaParameters
lambdaParametersEmpty = lambdaParameters nothing
  (list ([] :: [TTerm Py.LambdaParamNoDefault]))
  (list ([] :: [TTerm Py.LambdaParamWithDefault]))
  nothing

lambdaParametersSimple :: TTerm [Py.LambdaParamNoDefault] -> TTerm Py.LambdaParameters
lambdaParametersSimple params = lambdaParameters nothing params
  (list ([] :: [TTerm Py.LambdaParamWithDefault]))
  nothing

lambda_ :: TTerm Py.LambdaParameters -> TTerm Py.Expression -> TTerm Py.Lambda
lambda_ = Gen.lambda

list_ :: TTerm [Py.StarNamedExpression] -> TTerm Py.List
list_ = wrap Py._List

paramNoDefaultParametersSimple :: TTerm [Py.ParamNoDefault] -> TTerm Py.ParamNoDefaultParameters
paramNoDefaultParametersSimple noDefault = paramNoDefaultParameters noDefault (list ([] :: [TTerm Py.ParamWithDefault])) nothing

paramNoDefaultSimple :: TTerm Py.Param -> TTerm Py.ParamNoDefault
paramNoDefaultSimple p = paramNoDefault p nothing

paramSimple :: TTerm Py.Name -> TTerm Py.Param
paramSimple n = param n nothing

simpleTypeParameterSimple :: TTerm Py.Name -> TTerm Py.SimpleTypeParameter
simpleTypeParameterSimple n = simpleTypeParameter n nothing nothing

sliceSlice :: TTerm Py.SliceExpression -> TTerm Py.Slice
sliceSlice = inject Py._Slice Py._Slice_slice_

string_ :: TTerm String -> TTerm Py.QuoteStyle -> TTerm Py.String_
string_ val style = record Py._String [
  Py._String_value>>: val,
  Py._String_quoteStyle>>: style]

untypedAssignmentSimple :: TTerm [Py.StarTarget] -> TTerm Py.AnnotatedRhs -> TTerm Py.UntypedAssignment
untypedAssignmentSimple targets rhs = untypedAssignment targets rhs nothing

-- Expression conversion pipeline: Primary -> ... -> Expression

pyPrimaryToPyExpression :: TTerm Py.Primary -> TTerm Py.Expression
pyPrimaryToPyExpression p = expressionSimple (pyPrimaryToPyDisjunction p)

pyPrimaryToPyDisjunction :: TTerm Py.Primary -> TTerm Py.Disjunction
pyPrimaryToPyDisjunction p = wrap Py._Disjunction (list [pyPrimaryToPyConjunction p])

pyPrimaryToPyConjunction :: TTerm Py.Primary -> TTerm Py.Conjunction
pyPrimaryToPyConjunction p = wrap Py._Conjunction (list [pyPrimaryToPyInversion p])

pyPrimaryToPyInversion :: TTerm Py.Primary -> TTerm Py.Inversion
pyPrimaryToPyInversion p = inversionSimple (pyPrimaryToPyComparison p)

pyPrimaryToPyComparison :: TTerm Py.Primary -> TTerm Py.Comparison
pyPrimaryToPyComparison p = comparison (pyPrimaryToPyBitwiseOr p) (list ([] :: [TTerm Py.CompareOpBitwiseOrPair]))

pyPrimaryToPyBitwiseOr :: TTerm Py.Primary -> TTerm Py.BitwiseOr
pyPrimaryToPyBitwiseOr p = bitwiseOr nothing (pyPrimaryToPyBitwiseXor p)

pyPrimaryToPyBitwiseXor :: TTerm Py.Primary -> TTerm Py.BitwiseXor
pyPrimaryToPyBitwiseXor p = bitwiseXor nothing (pyPrimaryToPyBitwiseAnd p)

pyPrimaryToPyBitwiseAnd :: TTerm Py.Primary -> TTerm Py.BitwiseAnd
pyPrimaryToPyBitwiseAnd p = bitwiseAnd nothing (pyPrimaryToPyShiftExpression p)

pyPrimaryToPyShiftExpression :: TTerm Py.Primary -> TTerm Py.ShiftExpression
pyPrimaryToPyShiftExpression p = shiftExpression nothing (pyPrimaryToPySum p)

pyPrimaryToPySum :: TTerm Py.Primary -> TTerm Py.Sum
pyPrimaryToPySum p = Gen.sum nothing (pyPrimaryToPyTerm p)

pyPrimaryToPyTerm :: TTerm Py.Primary -> TTerm Py.Term
pyPrimaryToPyTerm p = Gen.term nothing (pyPrimaryToPyFactor p)

pyPrimaryToPyFactor :: TTerm Py.Primary -> TTerm Py.Factor
pyPrimaryToPyFactor p = factorSimple (pyPrimaryToPyPower p)

pyPrimaryToPyPower :: TTerm Py.Primary -> TTerm Py.Power
pyPrimaryToPyPower p = power (pyPrimaryToPyAwaitPrimary p) nothing

pyPrimaryToPyAwaitPrimary :: TTerm Py.Primary -> TTerm Py.AwaitPrimary
pyPrimaryToPyAwaitPrimary p = awaitPrimary false p

pyComparisonToPyExpression :: TTerm Py.Comparison -> TTerm Py.Expression
pyComparisonToPyExpression c = expressionSimple
  (wrap Py._Disjunction (list [
    wrap Py._Conjunction (list [
      inversionSimple c])]))

pyNameToPyPrimary :: TTerm Py.Name -> TTerm Py.Primary
pyNameToPyPrimary n = primarySimple (atomName n)

pyNameToPyExpression :: TTerm Py.Name -> TTerm Py.Expression
pyNameToPyExpression n = pyPrimaryToPyExpression (pyNameToPyPrimary n)

pyStringToPyExpression :: TTerm Py.String_ -> TTerm Py.Expression
pyStringToPyExpression s = pyPrimaryToPyExpression (primarySimple (atomString s))

raiseExpressionException :: TTerm Py.RaiseExpression -> TTerm Py.Expression
raiseExpressionException re = project Py._RaiseExpression Py._RaiseExpression_expression @@ re
