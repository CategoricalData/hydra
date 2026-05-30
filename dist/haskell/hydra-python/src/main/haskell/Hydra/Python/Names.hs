-- Note: this is an automatically generated file. Do not edit.
-- | Python naming utilities: encoding Hydra names as Python names

module Hydra.Python.Names where
import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Python.Environment as Environment
import qualified Hydra.Python.Language as Language
import qualified Hydra.Python.Serde as Serde
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
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
-- | Generate a constant name for a field definition
encodeConstantForFieldName :: t0 -> t1 -> Core.Name -> Syntax.Name
encodeConstantForFieldName env tname fname =
    Syntax.Name (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName fname))
-- | Generate a constant name for a type definition
encodeConstantForTypeName :: t0 -> t1 -> Syntax.Name
encodeConstantForTypeName env tname = Syntax.Name "TYPE_"
-- | Encode a name as a Python enum value (UPPER_SNAKE case)
encodeEnumValue :: Environment.PythonEnvironment -> Core.Name -> Syntax.Name
encodeEnumValue = encodeName False Util.CaseConventionUpperSnake
-- | Encode a name as a Python field name (lower_snake case)
encodeFieldName :: Environment.PythonEnvironment -> Core.Name -> Syntax.Name
encodeFieldName env fname = encodeName False Util.CaseConventionLowerSnake env fname
-- | Encode a Hydra name as a Python name
encodeName :: Bool -> Util.CaseConvention -> Environment.PythonEnvironment -> Core.Name -> Syntax.Name
encodeName isQualified conv env name =

      let namespaces = Environment.pythonEnvironmentNamespaces env
          focusPair = Util.namespacesFocus namespaces
          focusNs = Pairs.first focusPair
          boundVars = Pairs.second (Environment.pythonEnvironmentBoundTypeVariables env)
          qualName = Names.qualifyName name
          mns = Packaging.qualifiedNameModuleName qualName
          local = Packaging.qualifiedNameLocal qualName
          pyLocal = sanitizePythonName (Formatting.convertCase Util.CaseConventionCamel conv local)
          pyNs =
                  \nsVal -> Strings.intercalate "." (Lists.map (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake) (Strings.splitOn "." (Packaging.unModuleName nsVal)))
      in (Logic.ifElse isQualified (Maybes.maybe (Logic.ifElse (Equality.equal mns (Just focusNs)) (Syntax.Name (Logic.ifElse useFutureAnnotations pyLocal (Serde.escapePythonString True pyLocal))) (Maybes.maybe (Syntax.Name pyLocal) (\nsVal -> Syntax.Name (Strings.cat2 (pyNs nsVal) (Strings.cat2 "." pyLocal))) mns)) (\n -> n) (Maps.lookup name boundVars)) (Syntax.Name pyLocal))
-- | Encode a name as a fully qualified Python name
encodeNameQualified :: Environment.PythonEnvironment -> Core.Name -> Syntax.Name
encodeNameQualified env name =

      let namespaces = Environment.pythonEnvironmentNamespaces env
          focusPair = Util.namespacesFocus namespaces
          focusNs = Pairs.first focusPair
          boundVars = Pairs.second (Environment.pythonEnvironmentBoundTypeVariables env)
          qualName = Names.qualifyName name
          mns = Packaging.qualifiedNameModuleName qualName
          local = Packaging.qualifiedNameLocal qualName
          pyNs =
                  \nsVal -> Strings.intercalate "." (Lists.map (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake) (Strings.splitOn "." (Packaging.unModuleName nsVal)))
      in (Maybes.maybe (Logic.ifElse (Equality.equal mns (Just focusNs)) (Syntax.Name (Logic.ifElse useFutureAnnotations local (Serde.escapePythonString True local))) (Maybes.maybe (Syntax.Name (sanitizePythonName local)) (\nsVal -> Syntax.Name (Strings.cat2 (pyNs nsVal) (Strings.cat2 "." (sanitizePythonName local)))) mns)) (\n -> n) (Maps.lookup name boundVars))
-- | Encode a namespace as a Python dotted name
encodeNamespace :: Packaging.ModuleName -> Syntax.DottedName
encodeNamespace nsVal =
    Syntax.DottedName (Lists.map (\part -> Syntax.Name (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake part)) (Strings.splitOn "." (Packaging.unModuleName nsVal)))
-- | Encode a type variable name (capitalized)
encodeTypeVariable :: Core.Name -> Syntax.Name
encodeTypeVariable name = Syntax.Name (Formatting.capitalize (Core.unName name))
-- | Sanitize a string to be a valid Python name
sanitizePythonName :: String -> String
sanitizePythonName = Formatting.sanitizeWithUnderscores Language.pythonReservedWords
-- | Reference a term variable as a Python expression
termVariableReference :: Environment.PythonEnvironment -> Core.Name -> Syntax.Expression
termVariableReference = variableReference Util.CaseConventionLowerSnake False
-- | Reference a type variable as a Python expression
typeVariableReference :: Environment.PythonEnvironment -> Core.Name -> Syntax.Expression
typeVariableReference = variableReference Util.CaseConventionPascal False
-- | Whether to use __future__ annotations for forward references
useFutureAnnotations :: Bool
useFutureAnnotations = True
-- | Reference a variable as a Python expression
variableReference :: Util.CaseConvention -> Bool -> Environment.PythonEnvironment -> Core.Name -> Syntax.Expression
variableReference conv quoted env name =

      let pyName = encodeName True conv env name
          unquoted =
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
                                        Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName pyName))},
                                      Syntax.powerRhs = Nothing}))}}}}}},
                        Syntax.comparisonRhs = []})]])
          namespaces = Environment.pythonEnvironmentNamespaces env
          focusPair = Util.namespacesFocus namespaces
          focusNs = Pairs.first focusPair
          mns = Names.namespaceOf name
          sameNamespace = Maybes.maybe False (\ns -> Equality.equal ns focusNs) mns
      in (Logic.ifElse (Logic.and quoted sameNamespace) (Syntax.ExpressionSimple (Syntax.Disjunction [
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
                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomString (Syntax.String_ {
                              Syntax.stringValue = (Syntax.unName pyName),
                              Syntax.stringPrefix = Nothing,
                              Syntax.stringQuoteStyle = Syntax.QuoteStyleDouble})))},
                          Syntax.powerRhs = Nothing}))}}}}}},
            Syntax.comparisonRhs = []})]])) unquoted)
-- | Generate a variant name from type name and field name
variantName :: Bool -> Environment.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.Name
variantName isQualified env tname fname =
    encodeName isQualified Util.CaseConventionPascal env (Core.Name (Strings.cat2 (Core.unName tname) (Formatting.capitalize (Core.unName fname))))
