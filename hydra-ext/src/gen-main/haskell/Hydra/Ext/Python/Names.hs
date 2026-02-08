-- Note: this is an automatically generated file. Do not edit.

-- | Python naming utilities: encoding Hydra names as Python names

module Hydra.Ext.Python.Names where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Helpers as Helpers
import qualified Hydra.Ext.Python.Language as Language
import qualified Hydra.Ext.Python.Serde as Serde
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Whether to use __future__ annotations for forward references
useFutureAnnotations :: Bool
useFutureAnnotations = True

encodeConstantForFieldName :: (t0 -> Core.Name -> Core.Name -> Syntax.Name)
encodeConstantForFieldName env tname fname = (Syntax.Name (Strings.cat [
  Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionUpperSnake (Names.localNameOf tname),
  "__",
  (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName fname)),
  "__NAME"]))

encodeConstantForTypeName :: (t0 -> Core.Name -> Syntax.Name)
encodeConstantForTypeName env tname = (Syntax.Name (Strings.cat2 (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionUpperSnake (Names.localNameOf tname)) "__NAME"))

-- | Encode a name as a Python enum value (UPPER_SNAKE case)
encodeEnumValue :: (Helpers.PythonEnvironment -> Core.Name -> Syntax.Name)
encodeEnumValue = (encodeName False Util.CaseConventionUpperSnake)

-- | Encode a name as a Python field name (lower_snake case)
encodeFieldName :: (Helpers.PythonEnvironment -> Core.Name -> Syntax.Name)
encodeFieldName env fname = (encodeName False Util.CaseConventionLowerSnake env fname)

-- | Encode a Hydra name as a Python name
encodeName :: (Bool -> Util.CaseConvention -> Helpers.PythonEnvironment -> Core.Name -> Syntax.Name)
encodeName isQualified conv env name =  
  let namespaces = (Helpers.pythonEnvironmentNamespaces env) 
      focusPair = (Module.namespacesFocus namespaces)
      focusNs = (Pairs.first focusPair)
      boundVars = (Pairs.second (Helpers.pythonEnvironmentBoundTypeVariables env))
      qualName = (Names.qualifyName name)
      mns = (Module.qualifiedNameNamespace qualName)
      local = (Module.qualifiedNameLocal qualName)
      pyLocal = (sanitizePythonName (Formatting.convertCase Util.CaseConventionCamel conv local))
      pyNs = (\nsVal -> Strings.intercalate "." (Lists.map (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake) (Strings.splitOn "." (Module.unNamespace nsVal))))
  in (Logic.ifElse isQualified (Maybes.maybe (Logic.ifElse (Equality.equal mns (Just focusNs)) (Syntax.Name (Logic.ifElse useFutureAnnotations pyLocal (Serde.escapePythonString True pyLocal))) (Maybes.maybe (Syntax.Name pyLocal) (\nsVal -> Syntax.Name (Strings.cat2 (pyNs nsVal) (Strings.cat2 "." pyLocal))) mns)) (\n -> n) (Maps.lookup name boundVars)) (Syntax.Name pyLocal))

-- | Encode a name as a fully qualified Python name
encodeNameQualified :: (Helpers.PythonEnvironment -> Core.Name -> Syntax.Name)
encodeNameQualified env name =  
  let namespaces = (Helpers.pythonEnvironmentNamespaces env) 
      focusPair = (Module.namespacesFocus namespaces)
      focusNs = (Pairs.first focusPair)
      boundVars = (Pairs.second (Helpers.pythonEnvironmentBoundTypeVariables env))
      qualName = (Names.qualifyName name)
      mns = (Module.qualifiedNameNamespace qualName)
      local = (Module.qualifiedNameLocal qualName)
  in (Maybes.maybe (Logic.ifElse (Equality.equal mns (Just focusNs)) (Syntax.Name (Logic.ifElse useFutureAnnotations local (Serde.escapePythonString True local))) (Syntax.Name (Strings.intercalate "." (Lists.map sanitizePythonName (Strings.splitOn "." (Core.unName name)))))) (\n -> n) (Maps.lookup name boundVars))

-- | Encode a namespace as a Python dotted name
encodeNamespace :: (Module.Namespace -> Syntax.DottedName)
encodeNamespace nsVal = (Syntax.DottedName (Lists.map (\part -> Syntax.Name (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake part)) (Strings.splitOn "." (Module.unNamespace nsVal))))

-- | Encode a type variable name (capitalized)
encodeTypeVariable :: (Core.Name -> Syntax.Name)
encodeTypeVariable name = (Syntax.Name (Formatting.capitalize (Core.unName name)))

-- | Sanitize a string to be a valid Python name
sanitizePythonName :: (String -> String)
sanitizePythonName = (Formatting.sanitizeWithUnderscores Language.pythonReservedWords)

-- | Reference a term variable as a Python expression
termVariableReference :: (Helpers.PythonEnvironment -> Core.Name -> Syntax.Expression)
termVariableReference = (variableReference Util.CaseConventionLowerSnake False)

-- | Reference a type variable as a Python expression
typeVariableReference :: (Helpers.PythonEnvironment -> Core.Name -> Syntax.Expression)
typeVariableReference = (variableReference Util.CaseConventionPascal False)

-- | Generate a variant name from type name and field name
variantName :: (Bool -> Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.Name)
variantName isQualified env tname fname = (encodeName isQualified Util.CaseConventionPascal env (Core.Name (Strings.cat2 (Core.unName tname) (Formatting.capitalize (Core.unName fname)))))

-- | Reference a variable as a Python expression
variableReference :: (Util.CaseConvention -> Bool -> Helpers.PythonEnvironment -> Core.Name -> Syntax.Expression)
variableReference conv quoted env name =  
  let pyName = (encodeName True conv env name) 
      unquoted = (Syntax.ExpressionSimple (Syntax.Disjunction [
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
                  Syntax.comparisonRhs = []})]]))
      namespaces = (Helpers.pythonEnvironmentNamespaces env)
      focusPair = (Module.namespacesFocus namespaces)
      focusNs = (Pairs.first focusPair)
      mns = (Names.namespaceOf name)
      sameNamespace = (Maybes.maybe False (\ns -> Equality.equal ns focusNs) mns)
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
                          Syntax.stringQuoteStyle = Syntax.QuoteStyleDouble})))},
                      Syntax.powerRhs = Nothing}))}}}}}},
        Syntax.comparisonRhs = []})]])) unquoted)
