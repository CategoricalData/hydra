-- | Python naming utilities: encoding Hydra names as Python names.
-- Provides functions for converting Hydra names to Python naming conventions.

module Hydra.Ext.Sources.Python.Names where

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
import qualified Hydra.Ext.Sources.Python.Language as PyLanguage
import qualified Hydra.Ext.Dsl.Python.Syntax as PyDsl


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.python.names"

pyLanguageNs :: Namespace
pyLanguageNs = Namespace "hydra.ext.python.language"

module_ :: Module
module_ = Module ns elements
    [Names.ns, Formatting.ns, PySerde.ns, pyLanguageNs]
    (PyHelpersSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python naming utilities: encoding Hydra names as Python names"
  where
    elements = [
      toBinding useFutureAnnotations,
      toBinding encodeConstantForFieldName,
      toBinding encodeConstantForTypeName,
      toBinding encodeEnumValue,
      toBinding encodeFieldName,
      toBinding encodeName,
      toBinding encodeNameQualified,
      toBinding encodeNamespace,
      toBinding encodeTypeVariable,
      toBinding sanitizePythonName,
      toBinding termVariableReference,
      toBinding typeVariableReference,
      toBinding variantName,
      toBinding variableReference]

-- | Temporary flag for Python code generation - use __future__ annotations.
useFutureAnnotations :: TBinding Bool
useFutureAnnotations = def "useFutureAnnotations" $
  doc "Whether to use __future__ annotations for forward references" $
  true

-- | Encode a constant name for a field (e.g., TYPE_NAME__FIELD_NAME__NAME).
encodeConstantForFieldName :: TBinding (PyHelpers.PythonEnvironment -> Name -> Name -> Py.Name)
encodeConstantForFieldName = def "encodeConstantForFieldName" $
  doc "Generate a constant name for a field definition" $
  lambdas ["env", "tname", "fname"] $ wrap Py._Name $ Strings.cat $ list [
    Formatting.convertCase @@ Util.caseConventionPascal @@ Util.caseConventionUpperSnake @@ (Names.localNameOf @@ var "tname"),
    string "__",
    Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake @@ (Core.unName $ var "fname"),
    string "__NAME"]

-- | Encode a constant name for a type (e.g., TYPE_NAME__NAME).
encodeConstantForTypeName :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Name)
encodeConstantForTypeName = def "encodeConstantForTypeName" $
  doc "Generate a constant name for a type definition" $
  lambdas ["env", "tname"] $ wrap Py._Name $ Strings.cat2
    (Formatting.convertCase @@ Util.caseConventionPascal @@ Util.caseConventionUpperSnake @@ (Names.localNameOf @@ var "tname"))
    (string "__NAME")

-- | Encode an enum value name (UPPER_SNAKE case).
encodeEnumValue :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Name)
encodeEnumValue = def "encodeEnumValue" $
  doc "Encode a name as a Python enum value (UPPER_SNAKE case)" $
  encodeName @@ false @@ Util.caseConventionUpperSnake

-- | Encode a field name (lower_snake case).
encodeFieldName :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Name)
encodeFieldName = def "encodeFieldName" $
  doc "Encode a name as a Python field name (lower_snake case)" $
  lambdas ["env", "fname"] $ encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "fname"

-- | Encode a Hydra name as a Python name with the given case convention.
encodeName :: TBinding (Bool -> CaseConvention -> PyHelpers.PythonEnvironment -> Name -> Py.Name)
encodeName = def "encodeName" $
  doc "Encode a Hydra name as a Python name" $
  lambdas ["isQualified", "conv", "env", "name"] $ lets [
    -- Get the namespaces from the environment
    "namespaces">: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "env",
    -- Get the focus namespace (first element of the focus tuple)
    "focusPair">: Module.namespacesFocus (var "namespaces"),
    "focusNs">: Pairs.first $ var "focusPair",
    -- Get the bound type variables map from the environment
    "boundVars">: Pairs.second $ project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env",
    -- Qualify the name
    "qualName">: Names.qualifyName @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName",
    -- Convert local name with case convention and sanitize
    "pyLocal">: sanitizePythonName @@ (Formatting.convertCase @@ Util.caseConventionCamel @@ var "conv" @@ var "local"),
    -- Convert namespace to Python dotted path
    "pyNs">: lambda "nsVal" $ Strings.intercalate (string ".") $
      Lists.map (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionLowerSnake)
        (Strings.splitOn (string ".") (Core.unNamespace $ var "nsVal"))] $
    -- If qualified, check bound vars first, then namespace
    Logic.ifElse (var "isQualified")
      -- Check if name is a bound type variable
      (Maybes.maybe
        -- Not a bound type variable - check namespace
        (Logic.ifElse (Equality.equal (var "mns") (just $ var "focusNs"))
          -- Same namespace - use local name (possibly escaped for future annotations)
          (wrap Py._Name $ Logic.ifElse useFutureAnnotations (var "pyLocal") (PySerde.escapePythonString @@ true @@ var "pyLocal"))
          -- Different namespace or no namespace
          (Maybes.maybe
            -- No namespace - just use local name
            (wrap Py._Name $ var "pyLocal")
            -- Has namespace - use dotted path
            (lambda "nsVal" $ wrap Py._Name $ Strings.cat2 (var "pyNs" @@ var "nsVal") (Strings.cat2 (string ".") (var "pyLocal")))
            (var "mns")))
        -- Found in bound type variables - use the bound name
        (lambda "n" $ var "n")
        (Maps.lookup (var "name") (var "boundVars")))
      -- Not qualified - just use local name
      (wrap Py._Name $ var "pyLocal")

-- | Encode a qualified name for Python.
encodeNameQualified :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Name)
encodeNameQualified = def "encodeNameQualified" $
  doc "Encode a name as a fully qualified Python name" $
  lambdas ["env", "name"] $ lets [
    -- Get the namespaces from the environment
    "namespaces">: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "env",
    -- Get the focus namespace (first element of the focus tuple)
    "focusPair">: Module.namespacesFocus (var "namespaces"),
    "focusNs">: Pairs.first $ var "focusPair",
    -- Get the bound type variables map from the environment
    "boundVars">: Pairs.second $ project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_boundTypeVariables @@ var "env",
    -- Qualify the name
    "qualName">: Names.qualifyName @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName"] $
    -- Check if name is a bound type variable
    Maybes.maybe
      -- Not a bound type variable
      (Logic.ifElse (Equality.equal (var "mns") (just $ var "focusNs"))
        -- Same namespace - use local name (possibly escaped for future annotations)
        (wrap Py._Name $ Logic.ifElse useFutureAnnotations (var "local") (PySerde.escapePythonString @@ true @@ var "local"))
        -- Different namespace - use full dotted name with sanitization
        (wrap Py._Name $ Strings.intercalate (string ".") $
          Lists.map sanitizePythonName (Strings.splitOn (string ".") (Core.unName $ var "name"))))
      -- Found in bound type variables - use the bound name
      (lambda "n" $ var "n")
      (Maps.lookup (var "name") (var "boundVars"))

-- | Encode a namespace as a Python dotted name.
encodeNamespace :: TBinding (Namespace -> Py.DottedName)
encodeNamespace = def "encodeNamespace" $
  doc "Encode a namespace as a Python dotted name" $
  lambda "nsVal" $ wrap Py._DottedName $
    Lists.map
      (lambda "part" $ wrap Py._Name $ Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionLowerSnake @@ var "part")
      (Strings.splitOn (string ".") (Core.unNamespace $ var "nsVal"))

-- | Encode a type variable name (capitalized).
encodeTypeVariable :: TBinding (Name -> Py.Name)
encodeTypeVariable = def "encodeTypeVariable" $
  doc "Encode a type variable name (capitalized)" $
  lambda "name" $ wrap Py._Name $ Formatting.capitalize @@ (Core.unName $ var "name")

-- | Sanitize a string to be a valid Python name.
sanitizePythonName :: TBinding (String -> String)
sanitizePythonName = def "sanitizePythonName" $
  doc "Sanitize a string to be a valid Python name" $
  Formatting.sanitizeWithUnderscores @@ PyLanguage.pythonReservedWords

-- | Reference a term variable as a Python expression.
termVariableReference :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Expression)
termVariableReference = def "termVariableReference" $
  doc "Reference a term variable as a Python expression" $
  variableReference @@ Util.caseConventionLowerSnake @@ false

-- | Reference a type variable as a Python expression.
typeVariableReference :: TBinding (PyHelpers.PythonEnvironment -> Name -> Py.Expression)
typeVariableReference = def "typeVariableReference" $
  doc "Reference a type variable as a Python expression" $
  variableReference @@ Util.caseConventionPascal @@ false

-- | Generate a variant name by combining type name and field name.
variantName :: TBinding (Bool -> PyHelpers.PythonEnvironment -> Name -> Name -> Py.Name)
variantName = def "variantName" $
  doc "Generate a variant name from type name and field name" $
  lambdas ["isQualified", "env", "tname", "fname"] $
    encodeName @@ var "isQualified" @@ Util.caseConventionPascal @@ var "env" @@
      (wrap _Name $ Strings.cat2 (Core.unName $ var "tname") (Formatting.capitalize @@ (Core.unName $ var "fname")))

-- | Reference a variable as a Python expression with optional quoting.
variableReference :: TBinding (CaseConvention -> Bool -> PyHelpers.PythonEnvironment -> Name -> Py.Expression)
variableReference = def "variableReference" $
  doc "Reference a variable as a Python expression" $
  lambdas ["conv", "quoted", "env", "name"] $ lets [
    "pyName">: encodeName @@ true @@ var "conv" @@ var "env" @@ var "name",
    "unquoted">: PyDsl.pyNameToPyExpression (var "pyName"),
    -- Check if name is in the same namespace (for quoting)
    "namespaces">: project PyHelpers._PythonEnvironment PyHelpers._PythonEnvironment_namespaces @@ var "env",
    "focusPair">: Module.namespacesFocus (var "namespaces"),
    "focusNs">: Pairs.first $ var "focusPair",
    "mns">: Names.namespaceOf @@ var "name",
    "sameNamespace">: Maybes.maybe false (lambda "ns" $ Equality.equal (var "ns") (var "focusNs")) (var "mns")] $
    Logic.ifElse (Logic.and (var "quoted") (var "sameNamespace"))
      (PyDsl.pyStringToPyExpression $ PyDsl.doubleQuotedString (unwrap Py._Name @@ var "pyName"))
      (var "unquoted")
