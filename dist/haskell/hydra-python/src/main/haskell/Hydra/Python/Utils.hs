-- Note: this is an automatically generated file. Do not edit.

-- | Python utilities for constructing Python syntax trees

module Hydra.Python.Utils where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Python.Environment as Environment
import qualified Hydra.Python.Names as Names
import qualified Hydra.Python.Serde as Serde
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Annotate an expression with an optional comment using Annotated[]
annotatedExpression :: Maybe String -> Syntax.Expression -> Syntax.Expression
annotatedExpression mcomment expr =
    Maybes.maybe expr (\c -> pyPrimaryToPyExpression (primaryWithExpressionSlices (pyNameToPyPrimary (Syntax.Name "Annotated")) [
      expr,
      (doubleQuotedString c)])) mcomment

-- | Annotate a statement with an optional comment
annotatedStatement :: Maybe String -> Syntax.Statement -> Syntax.Statement
annotatedStatement mcomment stmt =
    Maybes.maybe stmt (\c -> Syntax.StatementAnnotated (Syntax.AnnotatedStatement {
      Syntax.annotatedStatementComment = c,
      Syntax.annotatedStatementStatement = stmt})) mcomment

-- | Create an assignment statement from name and annotated rhs
assignment :: Syntax.Name -> Syntax.AnnotatedRhs -> Syntax.Statement
assignment name rhs =
    pyAssignmentToPyStatement (Syntax.AssignmentUntyped (Syntax.UntypedAssignment {
      Syntax.untypedAssignmentTargets = [
        pyNameToPyStarTarget name],
      Syntax.untypedAssignmentRhs = rhs,
      Syntax.untypedAssignmentTypeComment = Nothing}))

-- | Create an assignment statement from name and expression
assignmentStatement :: Syntax.Name -> Syntax.Expression -> Syntax.Statement
assignmentStatement name expr = assignment name (pyExpressionToPyAnnotatedRhs expr)

-- | Create a cast expression: cast(type, expr)
castTo :: Syntax.Expression -> Syntax.Expression -> Syntax.Expression
castTo pytype pyexpr =
    functionCall (pyNameToPyPrimary (Syntax.Name "cast")) [
      pytype,
      pyexpr]

-- | Create a comment statement (triple-quoted string)
commentStatement :: String -> Syntax.Statement
commentStatement s = pyExpressionToPyStatement (tripleQuotedString s)

-- | Decode a Comparison to a Primary if possible
decodePyComparisonToPyAwaitPrimary :: Syntax.Comparison -> Maybe Syntax.Primary
decodePyComparisonToPyAwaitPrimary c =

      let rhs = Syntax.comparisonRhs c
          lhs = Syntax.comparisonLhs c
          orLhs = Syntax.bitwiseOrLhs lhs
          orRhs = Syntax.bitwiseOrRhs lhs
          xorLhs = Syntax.bitwiseXorLhs orRhs
          xorRhs = Syntax.bitwiseXorRhs orRhs
          andLhs = Syntax.bitwiseAndLhs xorRhs
          andRhs = Syntax.bitwiseAndRhs xorRhs
          shiftLhs = Syntax.shiftExpressionLhs andRhs
          shiftRhs = Syntax.shiftExpressionRhs andRhs
          sumLhs = Syntax.sumLhs shiftRhs
          sumRhs = Syntax.sumRhs shiftRhs
          termLhs = Syntax.termLhs sumRhs
          termRhs = Syntax.termRhs sumRhs
      in (Logic.ifElse (Logic.not (Lists.null rhs)) Nothing (Logic.ifElse (Maybes.isJust orLhs) Nothing (Logic.ifElse (Maybes.isJust xorLhs) Nothing (Logic.ifElse (Maybes.isJust andLhs) Nothing (Logic.ifElse (Maybes.isJust shiftLhs) Nothing (Logic.ifElse (Maybes.isJust sumLhs) Nothing (Logic.ifElse (Maybes.isJust termLhs) Nothing (case termRhs of
        Syntax.FactorSimple v0 -> decodePyPowerToPyPrimary v0
        _ -> Nothing))))))))

-- | Decode a Conjunction to a Primary if possible
decodePyConjunctionToPyPrimary :: Syntax.Conjunction -> Maybe Syntax.Primary
decodePyConjunctionToPyPrimary c =

      let inversions = Syntax.unConjunction c
      in (Logic.ifElse (Equality.equal (Lists.length inversions) 1) (Maybes.bind (Lists.maybeHead inversions) (\i -> decodePyInversionToPyPrimary i)) Nothing)

-- | Decode an Expression to a Primary if possible
decodePyExpressionToPyPrimary :: Syntax.Expression -> Maybe Syntax.Primary
decodePyExpressionToPyPrimary e =
    case e of
      Syntax.ExpressionSimple v0 ->
        let conjunctions = Syntax.unDisjunction v0
        in (Logic.ifElse (Equality.equal (Lists.length conjunctions) 1) (Maybes.bind (Lists.maybeHead conjunctions) (\c2 -> decodePyConjunctionToPyPrimary c2)) Nothing)
      _ -> Nothing

-- | Decode an Inversion to a Primary if possible
decodePyInversionToPyPrimary :: Syntax.Inversion -> Maybe Syntax.Primary
decodePyInversionToPyPrimary i =
    case i of
      Syntax.InversionSimple v0 -> decodePyComparisonToPyAwaitPrimary v0
      _ -> Nothing

-- | Decode a Power to a Primary if possible
decodePyPowerToPyPrimary :: Syntax.Power -> Maybe Syntax.Primary
decodePyPowerToPyPrimary p =

      let lhs = Syntax.powerLhs p
          await = Syntax.awaitPrimaryAwait lhs
          prim = Syntax.awaitPrimaryPrimary lhs
      in (Logic.ifElse await Nothing (Just prim))

-- | Create a dotted assignment statement: obj.attr = expr
dottedAssignmentStatement :: Syntax.Name -> Syntax.Name -> Syntax.Expression -> Syntax.Statement
dottedAssignmentStatement obj attr expr =

      let target =
              Syntax.StarTargetUnstarred (Syntax.TargetWithStarAtomProject (Syntax.TPrimaryAndName {
                Syntax.tPrimaryAndNamePrimary = (Syntax.TPrimaryAtom (Syntax.AtomName obj)),
                Syntax.tPrimaryAndNameName = attr}))
      in (pyAssignmentToPyStatement (Syntax.AssignmentUntyped (Syntax.UntypedAssignment {
        Syntax.untypedAssignmentTargets = [
          target],
        Syntax.untypedAssignmentRhs = (pyExpressionToPyAnnotatedRhs expr),
        Syntax.untypedAssignmentTypeComment = Nothing})))

-- | Create a double-quoted string expression
doubleQuotedString :: String -> Syntax.Expression
doubleQuotedString s = stringToPyExpression Syntax.QuoteStyleDouble s

-- | Find all namespaces referenced by a list of definitions, plus the core namespace
findNamespaces :: Packaging.Namespace -> [Packaging.Definition] -> Packaging.Namespaces Syntax.DottedName
findNamespaces focusNs defs =

      let coreNs = Packaging.Namespace "hydra.core"
          namespaces = Analysis.namespacesForDefinitions Names.encodeNamespace focusNs defs
      in (Logic.ifElse (Equality.equal (Packaging.unNamespace (Pairs.first (Packaging.namespacesFocus namespaces))) (Packaging.unNamespace coreNs)) namespaces (Packaging.Namespaces {
        Packaging.namespacesFocus = (Packaging.namespacesFocus namespaces),
        Packaging.namespacesMapping = (Maps.insert coreNs (Names.encodeNamespace coreNs) (Packaging.namespacesMapping namespaces))}))

-- | Create a function call expression
functionCall :: Syntax.Primary -> [Syntax.Expression] -> Syntax.Expression
functionCall func args = pyPrimaryToPyExpression (primaryWithRhs func (Syntax.PrimaryRhsCall (pyExpressionsToPyArgs args)))

getItemParams :: Syntax.Parameters
getItemParams =
    Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
      Syntax.paramNoDefaultParametersParamNoDefault = [
        Syntax.ParamNoDefault {
          Syntax.paramNoDefaultParam = Syntax.Param {
            Syntax.paramName = (Syntax.Name "cls"),
            Syntax.paramAnnotation = Nothing},
          Syntax.paramNoDefaultTypeComment = Nothing},
        Syntax.ParamNoDefault {
          Syntax.paramNoDefaultParam = Syntax.Param {
            Syntax.paramName = (Syntax.Name "item"),
            Syntax.paramAnnotation = Nothing},
          Syntax.paramNoDefaultTypeComment = Nothing}],
      Syntax.paramNoDefaultParametersParamWithDefault = [],
      Syntax.paramNoDefaultParametersStarEtc = Nothing})

-- | Create an indented block with optional comment
indentedBlock :: Maybe String -> [[Syntax.Statement]] -> Syntax.Block
indentedBlock mcomment stmts =

      let commentGroup = Maybes.maybe [] (\s -> [
            commentStatement s]) mcomment
          groups = Lists.filter (\g -> Logic.not (Lists.null g)) (Lists.cons commentGroup stmts)
      in (Logic.ifElse (Lists.null groups) (Syntax.BlockIndented [
        [
          Syntax.StatementSimple [
            pyExpressionToPySimpleStatement (pyAtomToPyExpression Syntax.AtomEllipsis)]]]) (Syntax.BlockIndented groups))

-- | Create a name with parameters
nameAndParams :: Syntax.Name -> [Syntax.Expression] -> Syntax.Expression
nameAndParams pyName params = primaryAndParams (pyNameToPyPrimary pyName) params

-- | Create a NewType statement
newtypeStatement :: Syntax.Name -> Maybe String -> Syntax.Expression -> Syntax.Statement
newtypeStatement name mcomment expr =
    annotatedStatement mcomment (assignmentStatement name (functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "NewType"))) [
      doubleQuotedString (Syntax.unName name),
      expr]))

-- | Build an or-expression from multiple primaries
orExpression :: [Syntax.Primary] -> Syntax.Expression
orExpression prims =

      let build =
              \prev -> \ps -> Maybes.maybe (Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = prev,
                Syntax.bitwiseOrRhs = (pyPrimaryToPyBitwiseXor (Syntax.PrimarySimple Syntax.AtomEllipsis))}) (\p -> Logic.ifElse (Lists.null (Pairs.second p)) (Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = prev,
                Syntax.bitwiseOrRhs = (pyPrimaryToPyBitwiseXor (Pairs.first p))}) (build (Just (Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = prev,
                Syntax.bitwiseOrRhs = (pyPrimaryToPyBitwiseXor (Pairs.first p))})) (Pairs.second p))) (Lists.uncons ps)
      in (pyBitwiseOrToPyExpression (build Nothing prims))

-- | Create a primary with parameters (subscript)
primaryAndParams :: Syntax.Primary -> [Syntax.Expression] -> Syntax.Expression
primaryAndParams prim params = pyPrimaryToPyExpression (primaryWithExpressionSlices prim params)

-- | Create a Primary with expression slices
primaryWithExpressionSlices :: Syntax.Primary -> [Syntax.Expression] -> Syntax.Primary
primaryWithExpressionSlices prim exprs =
    Maybes.fromMaybe prim (Maybes.map (\p -> primaryWithSlices prim (pyExpressionToPySlice (Pairs.first p)) (Lists.map (\e -> Syntax.SliceOrStarredExpressionSlice (pyExpressionToPySlice e)) (Pairs.second p))) (Lists.uncons exprs))

-- | Combine a Primary with a PrimaryRhs
primaryWithRhs :: Syntax.Primary -> Syntax.PrimaryRhs -> Syntax.Primary
primaryWithRhs prim rhs =
    Syntax.PrimaryCompound (Syntax.PrimaryWithRhs {
      Syntax.primaryWithRhsPrimary = prim,
      Syntax.primaryWithRhsRhs = rhs})

-- | Create a Primary with slices
primaryWithSlices :: Syntax.Primary -> Syntax.Slice -> [Syntax.SliceOrStarredExpression] -> Syntax.Primary
primaryWithSlices prim first rest =
    primaryWithRhs prim (Syntax.PrimaryRhsSlices (Syntax.Slices {
      Syntax.slicesHead = first,
      Syntax.slicesTail = rest}))

-- | Project a field from an expression
projectFromExpression :: Syntax.Expression -> Syntax.Name -> Syntax.Expression
projectFromExpression exp name =

      let prim = Syntax.PrimarySimple (Syntax.AtomGroup (Syntax.GroupExpression (Syntax.NamedExpressionSimple exp)))
      in (pyPrimaryToPyExpression (Syntax.PrimaryCompound (Syntax.PrimaryWithRhs {
        Syntax.primaryWithRhsPrimary = prim,
        Syntax.primaryWithRhsRhs = (Syntax.PrimaryRhsProject name)})))

-- | Convert an Assignment to a Statement
pyAssignmentToPyStatement :: Syntax.Assignment -> Syntax.Statement
pyAssignmentToPyStatement a = pySimpleStatementToPyStatement (Syntax.SimpleStatementAssignment a)

-- | Convert an Atom to an Expression
pyAtomToPyExpression :: Syntax.Atom -> Syntax.Expression
pyAtomToPyExpression atom = pyPrimaryToPyExpression (Syntax.PrimarySimple atom)

-- | Convert a BitwiseOr to a Conjunction
pyBitwiseOrToPyConjunction :: Syntax.BitwiseOr -> Syntax.Conjunction
pyBitwiseOrToPyConjunction bor =
    Syntax.Conjunction [
      Syntax.InversionSimple (Syntax.Comparison {
        Syntax.comparisonLhs = bor,
        Syntax.comparisonRhs = []})]

-- | Convert a BitwiseOr to an Expression
pyBitwiseOrToPyExpression :: Syntax.BitwiseOr -> Syntax.Expression
pyBitwiseOrToPyExpression bor = pyConjunctionToPyExpression (pyBitwiseOrToPyConjunction bor)

-- | Convert a ClassDefinition to a Statement
pyClassDefinitionToPyStatement :: Syntax.ClassDefinition -> Syntax.Statement
pyClassDefinitionToPyStatement cd = Syntax.StatementCompound (Syntax.CompoundStatementClassDef cd)

-- | Convert a ClosedPattern to Patterns
pyClosedPatternToPyPatterns :: Syntax.ClosedPattern -> Syntax.Patterns
pyClosedPatternToPyPatterns p = Syntax.PatternsPattern (Syntax.PatternOr (Syntax.OrPattern [
  p]))

-- | Convert a Conjunction to an Expression
pyConjunctionToPyExpression :: Syntax.Conjunction -> Syntax.Expression
pyConjunctionToPyExpression conj = Syntax.ExpressionSimple (Syntax.Disjunction [
  conj])

-- | Convert an Expression to a BitwiseOr, wrapping in parens if needed
pyExpressionToBitwiseOr :: Syntax.Expression -> Syntax.BitwiseOr
pyExpressionToBitwiseOr e =
    Syntax.BitwiseOr {
      Syntax.bitwiseOrLhs = Nothing,
      Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
        Syntax.bitwiseXorLhs = Nothing,
        Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
          Syntax.bitwiseAndLhs = Nothing,
          Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
            Syntax.shiftExpressionLhs = Nothing,
            Syntax.shiftExpressionRhs = Syntax.Sum {
              Syntax.sumLhs = Nothing,
              Syntax.sumRhs = Syntax.Term {
                Syntax.termLhs = Nothing,
                Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                  Syntax.powerLhs = Syntax.AwaitPrimary {
                    Syntax.awaitPrimaryAwait = False,
                    Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomGroup (Syntax.GroupExpression (Syntax.NamedExpressionSimple e))))},
                  Syntax.powerRhs = Nothing}))}}}}}}

-- | Convert an Expression to a Disjunction, wrapping in parens if needed
pyExpressionToDisjunction :: Syntax.Expression -> Syntax.Disjunction
pyExpressionToDisjunction e =
    case e of
      Syntax.ExpressionSimple v0 -> v0
      _ -> Syntax.Disjunction [
        pyPrimaryToPyConjunction (Syntax.PrimarySimple (Syntax.AtomGroup (Syntax.GroupExpression (Syntax.NamedExpressionSimple e))))]

-- | Convert an Expression to an AnnotatedRhs
pyExpressionToPyAnnotatedRhs :: Syntax.Expression -> Syntax.AnnotatedRhs
pyExpressionToPyAnnotatedRhs expr = Syntax.AnnotatedRhsStar [
  Syntax.StarExpressionSimple expr]

-- | Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary
pyExpressionToPyPrimary :: Syntax.Expression -> Syntax.Primary
pyExpressionToPyPrimary e =
    Maybes.maybe (Syntax.PrimarySimple (Syntax.AtomGroup (Syntax.GroupExpression (Syntax.NamedExpressionSimple e)))) (\prim -> prim) (decodePyExpressionToPyPrimary e)

-- | Convert an Expression to a SimpleStatement (as star expressions)
pyExpressionToPySimpleStatement :: Syntax.Expression -> Syntax.SimpleStatement
pyExpressionToPySimpleStatement expr = Syntax.SimpleStatementStarExpressions [
  Syntax.StarExpressionSimple expr]

-- | Convert an Expression to a Slice
pyExpressionToPySlice :: Syntax.Expression -> Syntax.Slice
pyExpressionToPySlice expr = Syntax.SliceNamed (Syntax.NamedExpressionSimple expr)

-- | Convert an Expression to a StarNamedExpression
pyExpressionToPyStarNamedExpression :: Syntax.Expression -> Syntax.StarNamedExpression
pyExpressionToPyStarNamedExpression expr = Syntax.StarNamedExpressionSimple (Syntax.NamedExpressionSimple expr)

-- | Convert an Expression to a Statement
pyExpressionToPyStatement :: Syntax.Expression -> Syntax.Statement
pyExpressionToPyStatement expr = pySimpleStatementToPyStatement (pyExpressionToPySimpleStatement expr)

-- | Convert a list of Expressions to Args
pyExpressionsToPyArgs :: [Syntax.Expression] -> Syntax.Args
pyExpressionsToPyArgs exprs =
    Syntax.Args {
      Syntax.argsPositional = (Lists.map (\e -> Syntax.PosArgExpression e) exprs),
      Syntax.argsKwargOrStarred = [],
      Syntax.argsKwargOrDoubleStarred = []}

-- | Create a Python list from expressions
pyList :: [Syntax.Expression] -> Syntax.List
pyList exprs = Syntax.List (Lists.map pyExpressionToPyStarNamedExpression exprs)

-- | Convert a Name to an Expression
pyNameToPyExpression :: Syntax.Name -> Syntax.Expression
pyNameToPyExpression name = pyPrimaryToPyExpression (pyNameToPyPrimary name)

-- | Convert a Name to a NamedExpression
pyNameToPyNamedExpression :: Syntax.Name -> Syntax.NamedExpression
pyNameToPyNamedExpression name = Syntax.NamedExpressionSimple (pyNameToPyExpression name)

-- | Convert a Name to a Primary (simple atom)
pyNameToPyPrimary :: Syntax.Name -> Syntax.Primary
pyNameToPyPrimary name = Syntax.PrimarySimple (Syntax.AtomName name)

-- | Convert a Name to a StarTarget
pyNameToPyStarTarget :: Syntax.Name -> Syntax.StarTarget
pyNameToPyStarTarget name = Syntax.StarTargetUnstarred (Syntax.TargetWithStarAtomAtom (Syntax.StarAtomName name))

-- | Convert a Name to a TypeParameter
pyNameToPyTypeParameter :: Syntax.Name -> Syntax.TypeParameter
pyNameToPyTypeParameter name =
    Syntax.TypeParameterSimple (Syntax.SimpleTypeParameter {
      Syntax.simpleTypeParameterName = name,
      Syntax.simpleTypeParameterBound = Nothing,
      Syntax.simpleTypeParameterDefault = Nothing})

-- | The Python None value as a Name
pyNone :: Syntax.Name
pyNone = Syntax.Name "None"

-- | Convert a Primary to a BitwiseOr
pyPrimaryToPyBitwiseOr :: Syntax.Primary -> Syntax.BitwiseOr
pyPrimaryToPyBitwiseOr prim =
    Syntax.BitwiseOr {
      Syntax.bitwiseOrLhs = Nothing,
      Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
        Syntax.bitwiseXorLhs = Nothing,
        Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
          Syntax.bitwiseAndLhs = Nothing,
          Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
            Syntax.shiftExpressionLhs = Nothing,
            Syntax.shiftExpressionRhs = Syntax.Sum {
              Syntax.sumLhs = Nothing,
              Syntax.sumRhs = Syntax.Term {
                Syntax.termLhs = Nothing,
                Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                  Syntax.powerLhs = Syntax.AwaitPrimary {
                    Syntax.awaitPrimaryAwait = False,
                    Syntax.awaitPrimaryPrimary = prim},
                  Syntax.powerRhs = Nothing}))}}}}}}

-- | Convert a Primary to a BitwiseXor
pyPrimaryToPyBitwiseXor :: Syntax.Primary -> Syntax.BitwiseXor
pyPrimaryToPyBitwiseXor prim =
    Syntax.BitwiseXor {
      Syntax.bitwiseXorLhs = Nothing,
      Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
        Syntax.bitwiseAndLhs = Nothing,
        Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
          Syntax.shiftExpressionLhs = Nothing,
          Syntax.shiftExpressionRhs = Syntax.Sum {
            Syntax.sumLhs = Nothing,
            Syntax.sumRhs = Syntax.Term {
              Syntax.termLhs = Nothing,
              Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                Syntax.powerLhs = Syntax.AwaitPrimary {
                  Syntax.awaitPrimaryAwait = False,
                  Syntax.awaitPrimaryPrimary = prim},
                Syntax.powerRhs = Nothing}))}}}}}

-- | Convert a Primary to a Conjunction
pyPrimaryToPyConjunction :: Syntax.Primary -> Syntax.Conjunction
pyPrimaryToPyConjunction prim = pyBitwiseOrToPyConjunction (pyPrimaryToPyBitwiseOr prim)

-- | Convert a Primary to an Expression
pyPrimaryToPyExpression :: Syntax.Primary -> Syntax.Expression
pyPrimaryToPyExpression prim = pyConjunctionToPyExpression (pyPrimaryToPyConjunction prim)

-- | Convert a Primary to a Slice
pyPrimaryToPySlice :: Syntax.Primary -> Syntax.Slice
pyPrimaryToPySlice prim = pyExpressionToPySlice (pyPrimaryToPyExpression prim)

-- | Convert a SimpleStatement to a Statement
pySimpleStatementToPyStatement :: Syntax.SimpleStatement -> Syntax.Statement
pySimpleStatementToPyStatement s = Syntax.StatementSimple [
  s]

-- | Create a raise AssertionError statement
raiseAssertionError :: String -> Syntax.Statement
raiseAssertionError msg =
    pySimpleStatementToPyStatement (Syntax.SimpleStatementRaise (Syntax.RaiseStatement (Just (Syntax.RaiseExpression {
      Syntax.raiseExpressionExpression = (functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "AssertionError"))) [
        doubleQuotedString msg]),
      Syntax.raiseExpressionFrom = Nothing}))))

-- | Create a raise TypeError statement
raiseTypeError :: String -> Syntax.Statement
raiseTypeError msg =
    pySimpleStatementToPyStatement (Syntax.SimpleStatementRaise (Syntax.RaiseStatement (Just (Syntax.RaiseExpression {
      Syntax.raiseExpressionExpression = (functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "TypeError"))) [
        doubleQuotedString msg]),
      Syntax.raiseExpressionFrom = Nothing}))))

-- | Create a return statement with a single expression
returnSingle :: Syntax.Expression -> Syntax.Statement
returnSingle expr =
    pySimpleStatementToPyStatement (Syntax.SimpleStatementReturn (Syntax.ReturnStatement [
      Syntax.StarExpressionSimple expr]))

selfOnlyParams :: Syntax.Parameters
selfOnlyParams =
    Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
      Syntax.paramNoDefaultParametersParamNoDefault = [
        Syntax.ParamNoDefault {
          Syntax.paramNoDefaultParam = Syntax.Param {
            Syntax.paramName = (Syntax.Name "self"),
            Syntax.paramAnnotation = Nothing},
          Syntax.paramNoDefaultTypeComment = Nothing}],
      Syntax.paramNoDefaultParametersParamWithDefault = [],
      Syntax.paramNoDefaultParametersStarEtc = Nothing})

selfOtherParams :: Syntax.Parameters
selfOtherParams =
    Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
      Syntax.paramNoDefaultParametersParamNoDefault = [
        Syntax.ParamNoDefault {
          Syntax.paramNoDefaultParam = Syntax.Param {
            Syntax.paramName = (Syntax.Name "self"),
            Syntax.paramAnnotation = Nothing},
          Syntax.paramNoDefaultTypeComment = Nothing},
        Syntax.ParamNoDefault {
          Syntax.paramNoDefaultParam = Syntax.Param {
            Syntax.paramName = (Syntax.Name "other"),
            Syntax.paramAnnotation = Nothing},
          Syntax.paramNoDefaultTypeComment = Nothing}],
      Syntax.paramNoDefaultParametersParamWithDefault = [],
      Syntax.paramNoDefaultParametersStarEtc = Nothing})

-- | Create a single-quoted string expression
singleQuotedString :: String -> Syntax.Expression
singleQuotedString s = stringToPyExpression Syntax.QuoteStyleSingle s

-- | Create a string expression with a given quote style
stringToPyExpression :: Syntax.QuoteStyle -> String -> Syntax.Expression
stringToPyExpression style s =
    pyAtomToPyExpression (Syntax.AtomString (Syntax.String_ {
      Syntax.stringValue = s,
      Syntax.stringQuoteStyle = style}))

-- | Current target Python version for code generation
targetPythonVersion :: Environment.PythonVersion
targetPythonVersion = Environment.PythonVersionPython310

-- | Create a triple-quoted string expression
tripleQuotedString :: String -> Syntax.Expression
tripleQuotedString s = stringToPyExpression Syntax.QuoteStyleTriple s

-- | Generate a type alias statement using PEP 695 syntax (Python 3.12+)
typeAliasStatement :: Syntax.Name -> [Syntax.TypeParameter] -> Maybe String -> Syntax.Expression -> Syntax.Statement
typeAliasStatement name tparams mcomment tyexpr =
    annotatedStatement mcomment (pySimpleStatementToPyStatement (Syntax.SimpleStatementTypeAlias (Syntax.TypeAlias {
      Syntax.typeAliasName = name,
      Syntax.typeAliasTypeParams = tparams,
      Syntax.typeAliasExpression = tyexpr})))

-- | Generate a type alias statement using Python 3.10-compatible syntax: Name: TypeAlias = "TypeExpression"
typeAliasStatement310 :: Syntax.Name -> t0 -> Maybe String -> Syntax.Expression -> Syntax.Statement
typeAliasStatement310 name _tparams mcomment tyexpr =

      let quotedExpr = doubleQuotedString (Serialization.printExpr (Serde.encodeExpression tyexpr))
      in (annotatedStatement mcomment (pyAssignmentToPyStatement (Syntax.AssignmentTyped (Syntax.TypedAssignment {
        Syntax.typedAssignmentLhs = (Syntax.SingleTargetName name),
        Syntax.typedAssignmentType = (Syntax.ExpressionSimple (Syntax.Disjunction [
          Syntax.Conjunction [
            Syntax.InversionSimple (Syntax.Comparison {
              Syntax.comparisonLhs = Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = Nothing,
                Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                  Syntax.bitwiseXorLhs = Nothing,
                  Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                    Syntax.bitwiseAndLhs = Nothing,
                    Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                      Syntax.shiftExpressionLhs = Nothing,
                      Syntax.shiftExpressionRhs = Syntax.Sum {
                        Syntax.sumLhs = Nothing,
                        Syntax.sumRhs = Syntax.Term {
                          Syntax.termLhs = Nothing,
                          Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                            Syntax.powerLhs = Syntax.AwaitPrimary {
                              Syntax.awaitPrimaryAwait = False,
                              Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "TypeAlias")))},
                            Syntax.powerRhs = Nothing}))}}}}}},
              Syntax.comparisonRhs = []})]])),
        Syntax.typedAssignmentRhs = (Just (pyExpressionToPyAnnotatedRhs quotedExpr))}))))

-- | Generate a subscriptable union class for Python 3.10
unionTypeClassStatements310 :: Syntax.Name -> Maybe String -> Syntax.Expression -> [Syntax.Statement] -> [Syntax.Statement]
unionTypeClassStatements310 name mcomment tyexpr extraStmts =

      let nameStr = Syntax.unName name
          metaName = Syntax.Name (Strings.cat2 (Strings.cat2 "_" nameStr) "Meta")
          docString = Serialization.printExpr (Serde.encodeExpression tyexpr)
          returnObject =
                  pySimpleStatementToPyStatement (Syntax.SimpleStatementReturn (Syntax.ReturnStatement [
                    Syntax.StarExpressionSimple (Syntax.ExpressionSimple (Syntax.Disjunction [
                      Syntax.Conjunction [
                        Syntax.InversionSimple (Syntax.Comparison {
                          Syntax.comparisonLhs = Syntax.BitwiseOr {
                            Syntax.bitwiseOrLhs = Nothing,
                            Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                              Syntax.bitwiseXorLhs = Nothing,
                              Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                Syntax.bitwiseAndLhs = Nothing,
                                Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                  Syntax.shiftExpressionLhs = Nothing,
                                  Syntax.shiftExpressionRhs = Syntax.Sum {
                                    Syntax.sumLhs = Nothing,
                                    Syntax.sumRhs = Syntax.Term {
                                      Syntax.termLhs = Nothing,
                                      Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                        Syntax.powerLhs = Syntax.AwaitPrimary {
                                          Syntax.awaitPrimaryAwait = False,
                                          Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "object")))},
                                        Syntax.powerRhs = Nothing}))}}}}}},
                          Syntax.comparisonRhs = []})]]))]))
          getItemMethod =
                  Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                    Syntax.functionDefinitionDecorators = Nothing,
                    Syntax.functionDefinitionRaw = Syntax.FunctionDefRaw {
                      Syntax.functionDefRawAsync = False,
                      Syntax.functionDefRawName = (Syntax.Name "__getitem__"),
                      Syntax.functionDefRawTypeParams = [],
                      Syntax.functionDefRawParams = (Just getItemParams),
                      Syntax.functionDefRawReturnType = Nothing,
                      Syntax.functionDefRawFuncTypeComment = Nothing,
                      Syntax.functionDefRawBlock = (indentedBlock Nothing [
                        [
                          returnObject]])}}))
          metaClass =
                  pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
                    Syntax.classDefinitionDecorators = Nothing,
                    Syntax.classDefinitionName = metaName,
                    Syntax.classDefinitionTypeParams = [],
                    Syntax.classDefinitionArguments = (Just (pyExpressionsToPyArgs [
                      Syntax.ExpressionSimple (Syntax.Disjunction [
                        Syntax.Conjunction [
                          Syntax.InversionSimple (Syntax.Comparison {
                            Syntax.comparisonLhs = Syntax.BitwiseOr {
                              Syntax.bitwiseOrLhs = Nothing,
                              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                                Syntax.bitwiseXorLhs = Nothing,
                                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                  Syntax.bitwiseAndLhs = Nothing,
                                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                    Syntax.shiftExpressionLhs = Nothing,
                                    Syntax.shiftExpressionRhs = Syntax.Sum {
                                      Syntax.sumLhs = Nothing,
                                      Syntax.sumRhs = Syntax.Term {
                                        Syntax.termLhs = Nothing,
                                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                          Syntax.powerLhs = Syntax.AwaitPrimary {
                                            Syntax.awaitPrimaryAwait = False,
                                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "type")))},
                                          Syntax.powerRhs = Nothing}))}}}}}},
                            Syntax.comparisonRhs = []})]])])),
                    Syntax.classDefinitionBody = (indentedBlock Nothing [
                      [
                        getItemMethod]])})
          docStmt = pyExpressionToPyStatement (tripleQuotedString docString)
          bodyGroups =
                  Logic.ifElse (Lists.null extraStmts) (
                    let passStmt = pySimpleStatementToPyStatement Syntax.SimpleStatementPass
                    in [
                      [
                        docStmt],
                      [
                        passStmt]]) [
                    [
                      docStmt],
                    extraStmts]
          metaclassArg =
                  Syntax.Kwarg {
                    Syntax.kwargName = (Syntax.Name "metaclass"),
                    Syntax.kwargValue = (Syntax.ExpressionSimple (Syntax.Disjunction [
                      Syntax.Conjunction [
                        Syntax.InversionSimple (Syntax.Comparison {
                          Syntax.comparisonLhs = Syntax.BitwiseOr {
                            Syntax.bitwiseOrLhs = Nothing,
                            Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                              Syntax.bitwiseXorLhs = Nothing,
                              Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                Syntax.bitwiseAndLhs = Nothing,
                                Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                  Syntax.shiftExpressionLhs = Nothing,
                                  Syntax.shiftExpressionRhs = Syntax.Sum {
                                    Syntax.sumLhs = Nothing,
                                    Syntax.sumRhs = Syntax.Term {
                                      Syntax.termLhs = Nothing,
                                      Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                        Syntax.powerLhs = Syntax.AwaitPrimary {
                                          Syntax.awaitPrimaryAwait = False,
                                          Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName metaName))},
                                        Syntax.powerRhs = Nothing}))}}}}}},
                          Syntax.comparisonRhs = []})]]))}
          unionClass =
                  annotatedStatement mcomment (pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
                    Syntax.classDefinitionDecorators = Nothing,
                    Syntax.classDefinitionName = name,
                    Syntax.classDefinitionTypeParams = [],
                    Syntax.classDefinitionArguments = (Just (Syntax.Args {
                      Syntax.argsPositional = [],
                      Syntax.argsKwargOrStarred = [
                        Syntax.KwargOrStarredKwarg metaclassArg],
                      Syntax.argsKwargOrDoubleStarred = []})),
                    Syntax.classDefinitionBody = (indentedBlock Nothing bodyGroups)}))
      in [
        metaClass,
        unionClass]

-- | Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants
unitVariantMethods :: Syntax.Name -> [Syntax.Statement]
unitVariantMethods className =

      let classNameStr = Syntax.unName className
          slotsStmt =
                  assignmentStatement (Syntax.Name "__slots__") (pyPrimaryToPyExpression (Syntax.PrimarySimple (Syntax.AtomTuple (Syntax.Tuple []))))
          returnIsinstance =
                  pySimpleStatementToPyStatement (Syntax.SimpleStatementReturn (Syntax.ReturnStatement [
                    Syntax.StarExpressionSimple (functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "isinstance"))) [
                      Syntax.ExpressionSimple (Syntax.Disjunction [
                        Syntax.Conjunction [
                          Syntax.InversionSimple (Syntax.Comparison {
                            Syntax.comparisonLhs = Syntax.BitwiseOr {
                              Syntax.bitwiseOrLhs = Nothing,
                              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                                Syntax.bitwiseXorLhs = Nothing,
                                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                  Syntax.bitwiseAndLhs = Nothing,
                                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                    Syntax.shiftExpressionLhs = Nothing,
                                    Syntax.shiftExpressionRhs = Syntax.Sum {
                                      Syntax.sumLhs = Nothing,
                                      Syntax.sumRhs = Syntax.Term {
                                        Syntax.termLhs = Nothing,
                                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                          Syntax.powerLhs = Syntax.AwaitPrimary {
                                            Syntax.awaitPrimaryAwait = False,
                                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "other")))},
                                          Syntax.powerRhs = Nothing}))}}}}}},
                            Syntax.comparisonRhs = []})]]),
                      (Syntax.ExpressionSimple (Syntax.Disjunction [
                        Syntax.Conjunction [
                          Syntax.InversionSimple (Syntax.Comparison {
                            Syntax.comparisonLhs = Syntax.BitwiseOr {
                              Syntax.bitwiseOrLhs = Nothing,
                              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                                Syntax.bitwiseXorLhs = Nothing,
                                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                  Syntax.bitwiseAndLhs = Nothing,
                                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                    Syntax.shiftExpressionLhs = Nothing,
                                    Syntax.shiftExpressionRhs = Syntax.Sum {
                                      Syntax.sumLhs = Nothing,
                                      Syntax.sumRhs = Syntax.Term {
                                        Syntax.termLhs = Nothing,
                                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                          Syntax.powerLhs = Syntax.AwaitPrimary {
                                            Syntax.awaitPrimaryAwait = False,
                                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName className))},
                                          Syntax.powerRhs = Nothing}))}}}}}},
                            Syntax.comparisonRhs = []})]]))])]))
          eqMethod =
                  Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                    Syntax.functionDefinitionDecorators = Nothing,
                    Syntax.functionDefinitionRaw = Syntax.FunctionDefRaw {
                      Syntax.functionDefRawAsync = False,
                      Syntax.functionDefRawName = (Syntax.Name "__eq__"),
                      Syntax.functionDefRawTypeParams = [],
                      Syntax.functionDefRawParams = (Just selfOtherParams),
                      Syntax.functionDefRawReturnType = Nothing,
                      Syntax.functionDefRawFuncTypeComment = Nothing,
                      Syntax.functionDefRawBlock = (indentedBlock Nothing [
                        [
                          returnIsinstance]])}}))
          returnHash =
                  pySimpleStatementToPyStatement (Syntax.SimpleStatementReturn (Syntax.ReturnStatement [
                    Syntax.StarExpressionSimple (functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "hash"))) [
                      doubleQuotedString classNameStr])]))
          hashMethod =
                  Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                    Syntax.functionDefinitionDecorators = Nothing,
                    Syntax.functionDefinitionRaw = Syntax.FunctionDefRaw {
                      Syntax.functionDefRawAsync = False,
                      Syntax.functionDefRawName = (Syntax.Name "__hash__"),
                      Syntax.functionDefRawTypeParams = [],
                      Syntax.functionDefRawParams = (Just selfOnlyParams),
                      Syntax.functionDefRawReturnType = Nothing,
                      Syntax.functionDefRawFuncTypeComment = Nothing,
                      Syntax.functionDefRawBlock = (indentedBlock Nothing [
                        [
                          returnHash]])}}))
      in [
        slotsStmt,
        eqMethod,
        hashMethod]
