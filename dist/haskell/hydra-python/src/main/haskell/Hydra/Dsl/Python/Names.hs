-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.python.names

module Hydra.Dsl.Python.Names where

import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.Formatting as DslFormatting
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Names as DslNames
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Python.Environment as PythonEnvironment
import qualified Hydra.Dsl.Python.Syntax as PythonSyntax
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
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
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Python.Environment as Environment
import qualified Hydra.Python.Language as Language
import qualified Hydra.Python.Names as PythonNames
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
import qualified Data.Map as M

-- | DSL reference to hydra.python.names.encodeConstantForFieldName
encodeConstantForFieldName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeConstantForFieldName arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeConstantForFieldName")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.python.names.encodeConstantForTypeName
encodeConstantForTypeName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm Syntax.Name
encodeConstantForTypeName arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeConstantForTypeName")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.encodeEnumValue
encodeEnumValue :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeEnumValue arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeEnumValue")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.encodeFieldName
encodeFieldName :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeFieldName arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeFieldName")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.encodeName
encodeName :: Typed.TypedTerm Bool -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeName arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeName")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.python.names.encodeNameQualified
encodeNameQualified :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeNameQualified arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeNameQualified")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.encodeNamespace
encodeNamespace :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Syntax.DottedName
encodeNamespace arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeNamespace")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.names.encodeNamespaceStringWithOverrides
encodeNamespaceStringWithOverrides :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm String
encodeNamespaceStringWithOverrides arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeNamespaceStringWithOverrides")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.names.encodeNamespaceWithOverrides
encodeNamespaceWithOverrides :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Syntax.DottedName
encodeNamespaceWithOverrides arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeNamespaceWithOverrides")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.names.encodeTypeVariable
encodeTypeVariable :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
encodeTypeVariable arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.encodeTypeVariable")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.names.overlayPythonModuleAliases
overlayPythonModuleAliases :: Typed.TypedTerm (M.Map Packaging.ModuleName String)
overlayPythonModuleAliases = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.names.overlayPythonModuleAliases"))

-- | DSL reference to hydra.python.names.sanitizePythonName
sanitizePythonName :: Typed.TypedTerm String -> Typed.TypedTerm String
sanitizePythonName arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.sanitizePythonName")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.python.names.termVariableReference
termVariableReference :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Expression
termVariableReference arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.termVariableReference")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.typeVariableReference
typeVariableReference :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Expression
typeVariableReference arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.typeVariableReference")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.python.names.useFutureAnnotations
useFutureAnnotations :: Typed.TypedTerm Bool
useFutureAnnotations = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.python.names.useFutureAnnotations"))

-- | DSL reference to hydra.python.names.variableReference
variableReference :: Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Expression
variableReference arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.variableReference")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.python.names.variantName
variantName :: Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Syntax.Name
variantName arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.python.names.variantName")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))
