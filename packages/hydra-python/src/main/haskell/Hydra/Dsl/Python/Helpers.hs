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

argsPositionalOnly :: TypedTerm [Py.PosArg] -> TypedTerm Py.Args
argsPositionalOnly pos = args pos
  (list ([] :: [TypedTerm Py.KwargOrStarred]))
  (list ([] :: [TypedTerm Py.KwargOrDoubleStarred]))

classPatternSimple :: TypedTerm Py.NameOrAttribute -> TypedTerm Py.ClassPattern
classPatternSimple nameOrAttr = classPattern nameOrAttr nothing nothing

classPatternWithKeywords :: TypedTerm Py.NameOrAttribute -> TypedTerm Py.KeywordPatterns -> TypedTerm Py.ClassPattern
classPatternWithKeywords nameOrAttr kwPatterns = classPattern nameOrAttr nothing (just kwPatterns)

compPairEq :: TypedTerm Py.BitwiseOr -> TypedTerm Py.CompareOpBitwiseOrPair
compPairEq rhs = compareOpBitwiseOrPair compareOpEq rhs

doubleQuotedString :: TypedTerm String -> TypedTerm Py.String_
doubleQuotedString val = string_ val quoteStyleDouble

functionDefRawSimple :: TypedTerm Py.Name -> TypedTerm (Maybe Py.Parameters) -> TypedTerm Py.Block -> TypedTerm Py.FunctionDefRaw
functionDefRawSimple name_ params block_ = functionDefRaw false name_ (list ([] :: [TypedTerm Py.TypeParameter])) params nothing nothing block_

functionDefinitionSimple :: TypedTerm Py.FunctionDefRaw -> TypedTerm Py.FunctionDefinition
functionDefinitionSimple raw = functionDefinition nothing raw

lambdaParametersEmpty :: TypedTerm Py.LambdaParameters
lambdaParametersEmpty = lambdaParameters nothing
  (list ([] :: [TypedTerm Py.LambdaParamNoDefault]))
  (list ([] :: [TypedTerm Py.LambdaParamWithDefault]))
  nothing

lambdaParametersSimple :: TypedTerm [Py.LambdaParamNoDefault] -> TypedTerm Py.LambdaParameters
lambdaParametersSimple params = lambdaParameters nothing params
  (list ([] :: [TypedTerm Py.LambdaParamWithDefault]))
  nothing

lambda_ :: TypedTerm Py.LambdaParameters -> TypedTerm Py.Expression -> TypedTerm Py.Lambda
lambda_ = Gen.lambda

list_ :: TypedTerm [Py.StarNamedExpression] -> TypedTerm Py.List
list_ = wrap Py._List

paramNoDefaultParametersSimple :: TypedTerm [Py.ParamNoDefault] -> TypedTerm Py.ParamNoDefaultParameters
paramNoDefaultParametersSimple noDefault = paramNoDefaultParameters noDefault (list ([] :: [TypedTerm Py.ParamWithDefault])) nothing

paramNoDefaultSimple :: TypedTerm Py.Param -> TypedTerm Py.ParamNoDefault
paramNoDefaultSimple p = paramNoDefault p nothing

paramSimple :: TypedTerm Py.Name -> TypedTerm Py.Param
paramSimple n = param n nothing

simpleTypeParameterSimple :: TypedTerm Py.Name -> TypedTerm Py.SimpleTypeParameter
simpleTypeParameterSimple n = simpleTypeParameter n nothing nothing

sliceSlice :: TypedTerm Py.SliceExpression -> TypedTerm Py.Slice
sliceSlice = inject Py._Slice Py._Slice_slice_

string_ :: TypedTerm String -> TypedTerm Py.QuoteStyle -> TypedTerm Py.String_
string_ val style = record Py._String [
  Py._String_value>>: val,
  Py._String_prefix>>: nothing,
  Py._String_quoteStyle>>: style]

-- Like `string_`, but with an explicit prefix (`r` / `b` / `rb` / `u`) attached.
-- Used by raw triple-quoted docstrings to suppress backslash-escape interpretation.
prefixedString_ :: TypedTerm Py.StringPrefix -> TypedTerm String -> TypedTerm Py.QuoteStyle -> TypedTerm Py.String_
prefixedString_ prefix_ val style = record Py._String [
  Py._String_value>>: val,
  Py._String_prefix>>: just prefix_,
  Py._String_quoteStyle>>: style]

untypedAssignmentSimple :: TypedTerm [Py.StarTarget] -> TypedTerm Py.AnnotatedRhs -> TypedTerm Py.UntypedAssignment
untypedAssignmentSimple targets rhs = untypedAssignment targets rhs nothing

-- Expression conversion pipeline: Primary -> ... -> Expression

pyPrimaryToPyExpression :: TypedTerm Py.Primary -> TypedTerm Py.Expression
pyPrimaryToPyExpression p = expressionSimple (pyPrimaryToPyDisjunction p)

pyPrimaryToPyDisjunction :: TypedTerm Py.Primary -> TypedTerm Py.Disjunction
pyPrimaryToPyDisjunction p = wrap Py._Disjunction (list [pyPrimaryToPyConjunction p])

pyPrimaryToPyConjunction :: TypedTerm Py.Primary -> TypedTerm Py.Conjunction
pyPrimaryToPyConjunction p = wrap Py._Conjunction (list [pyPrimaryToPyInversion p])

pyPrimaryToPyInversion :: TypedTerm Py.Primary -> TypedTerm Py.Inversion
pyPrimaryToPyInversion p = inversionSimple (pyPrimaryToPyComparison p)

pyPrimaryToPyComparison :: TypedTerm Py.Primary -> TypedTerm Py.Comparison
pyPrimaryToPyComparison p = comparison (pyPrimaryToPyBitwiseOr p) (list ([] :: [TypedTerm Py.CompareOpBitwiseOrPair]))

pyPrimaryToPyBitwiseOr :: TypedTerm Py.Primary -> TypedTerm Py.BitwiseOr
pyPrimaryToPyBitwiseOr p = bitwiseOr nothing (pyPrimaryToPyBitwiseXor p)

pyPrimaryToPyBitwiseXor :: TypedTerm Py.Primary -> TypedTerm Py.BitwiseXor
pyPrimaryToPyBitwiseXor p = bitwiseXor nothing (pyPrimaryToPyBitwiseAnd p)

pyPrimaryToPyBitwiseAnd :: TypedTerm Py.Primary -> TypedTerm Py.BitwiseAnd
pyPrimaryToPyBitwiseAnd p = bitwiseAnd nothing (pyPrimaryToPyShiftExpression p)

pyPrimaryToPyShiftExpression :: TypedTerm Py.Primary -> TypedTerm Py.ShiftExpression
pyPrimaryToPyShiftExpression p = shiftExpression nothing (pyPrimaryToPySum p)

pyPrimaryToPySum :: TypedTerm Py.Primary -> TypedTerm Py.Sum
pyPrimaryToPySum p = Gen.sum nothing (pyPrimaryToPyTerm p)

pyPrimaryToPyTerm :: TypedTerm Py.Primary -> TypedTerm Py.Term
pyPrimaryToPyTerm p = Gen.term nothing (pyPrimaryToPyFactor p)

pyPrimaryToPyFactor :: TypedTerm Py.Primary -> TypedTerm Py.Factor
pyPrimaryToPyFactor p = factorSimple (pyPrimaryToPyPower p)

pyPrimaryToPyPower :: TypedTerm Py.Primary -> TypedTerm Py.Power
pyPrimaryToPyPower p = power (pyPrimaryToPyAwaitPrimary p) nothing

pyPrimaryToPyAwaitPrimary :: TypedTerm Py.Primary -> TypedTerm Py.AwaitPrimary
pyPrimaryToPyAwaitPrimary p = awaitPrimary false p

pyComparisonToPyExpression :: TypedTerm Py.Comparison -> TypedTerm Py.Expression
pyComparisonToPyExpression c = expressionSimple
  (wrap Py._Disjunction (list [
    wrap Py._Conjunction (list [
      inversionSimple c])]))

pyNameToPyPrimary :: TypedTerm Py.Name -> TypedTerm Py.Primary
pyNameToPyPrimary n = primarySimple (atomName n)

pyNameToPyExpression :: TypedTerm Py.Name -> TypedTerm Py.Expression
pyNameToPyExpression n = pyPrimaryToPyExpression (pyNameToPyPrimary n)

pyStringToPyExpression :: TypedTerm Py.String_ -> TypedTerm Py.Expression
pyStringToPyExpression s = pyPrimaryToPyExpression (primarySimple (atomString s))

raiseExpressionException :: TypedTerm Py.RaiseExpression -> TypedTerm Py.Expression
raiseExpressionException re = project Py._RaiseExpression Py._RaiseExpression_expression @@ re
