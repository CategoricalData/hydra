-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.analysis

module Hydra.Dsl.Analysis where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Ast as Ast
import qualified Hydra.Checking as Checking
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Annotations as DslAnnotations
import qualified Hydra.Dsl.Arity as DslArity
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Checking as DslChecking
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Constants as DslConstants
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Dependencies as DslDependencies
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as DslErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Lexical as DslLexical
import qualified Hydra.Dsl.Names as DslNames
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Predicates as DslPredicates
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Rewriting as DslRewriting
import qualified Hydra.Dsl.Scoping as DslScoping
import qualified Hydra.Dsl.Strip as DslStrip
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variables as DslVariables
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as ErrorChecking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Strip as Strip
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S

-- | DSL reference to hydra.analysis.addNamesToModuleNames
addNamesToModuleNames :: Typed.TypedTerm (Packaging.ModuleName -> t0) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (Util.ModuleNames t0) -> Typed.TypedTerm (Util.ModuleNames t0)
addNamesToModuleNames arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.addNamesToModuleNames")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.analysis.analyzeFunctionTerm
analyzeFunctionTerm :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t0) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTerm arg0 arg1 arg2 arg3 arg4 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.analyzeFunctionTerm")),
              Core.applicationArgument = (Typed.unTypedTerm arg0)})),
            Core.applicationArgument = (Typed.unTypedTerm arg1)})),
          Core.applicationArgument = (Typed.unTypedTerm arg2)})),
        Core.applicationArgument = (Typed.unTypedTerm arg3)})),
      Core.applicationArgument = (Typed.unTypedTerm arg4)}))

-- | DSL reference to hydra.analysis.analyzeFunctionTermWith
analyzeFunctionTermWith :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t0) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWith arg0 arg1 arg2 arg3 arg4 arg5 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.analyzeFunctionTermWith")),
                Core.applicationArgument = (Typed.unTypedTerm arg0)})),
              Core.applicationArgument = (Typed.unTypedTerm arg1)})),
            Core.applicationArgument = (Typed.unTypedTerm arg2)})),
          Core.applicationArgument = (Typed.unTypedTerm arg3)})),
        Core.applicationArgument = (Typed.unTypedTerm arg4)})),
      Core.applicationArgument = (Typed.unTypedTerm arg5)}))

-- | DSL reference to hydra.analysis.analyzeFunctionTermWithFinish
analyzeFunctionTermWithFinish :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm t0 -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWithFinish arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.analyzeFunctionTermWithFinish")),
                      Core.applicationArgument = (Typed.unTypedTerm arg0)})),
                    Core.applicationArgument = (Typed.unTypedTerm arg1)})),
                  Core.applicationArgument = (Typed.unTypedTerm arg2)})),
                Core.applicationArgument = (Typed.unTypedTerm arg3)})),
              Core.applicationArgument = (Typed.unTypedTerm arg4)})),
            Core.applicationArgument = (Typed.unTypedTerm arg5)})),
          Core.applicationArgument = (Typed.unTypedTerm arg6)})),
        Core.applicationArgument = (Typed.unTypedTerm arg7)})),
      Core.applicationArgument = (Typed.unTypedTerm arg8)}))

-- | DSL reference to hydra.analysis.analyzeFunctionTermWithGather
analyzeFunctionTermWithGather :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t0) -> Typed.TypedTerm Bool -> Typed.TypedTerm t0 -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t1 (Typing.FunctionStructure t0))
analyzeFunctionTermWithGather arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.analyzeFunctionTermWithGather")),
                            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
                          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
                        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
                      Core.applicationArgument = (Typed.unTypedTerm arg3)})),
                    Core.applicationArgument = (Typed.unTypedTerm arg4)})),
                  Core.applicationArgument = (Typed.unTypedTerm arg5)})),
                Core.applicationArgument = (Typed.unTypedTerm arg6)})),
              Core.applicationArgument = (Typed.unTypedTerm arg7)})),
            Core.applicationArgument = (Typed.unTypedTerm arg8)})),
          Core.applicationArgument = (Typed.unTypedTerm arg9)})),
        Core.applicationArgument = (Typed.unTypedTerm arg10)})),
      Core.applicationArgument = (Typed.unTypedTerm arg11)}))

-- | DSL reference to hydra.analysis.definitionDependencyModuleNames
definitionDependencyModuleNames :: Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm (S.Set Packaging.ModuleName)
definitionDependencyModuleNames arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.definitionDependencyModuleNames")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.analysis.dependencyModuleNames
dependencyModuleNames :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (Either Errors.Error (S.Set Packaging.ModuleName))
dependencyModuleNames arg0 arg1 arg2 arg3 arg4 arg5 arg6 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.dependencyModuleNames")),
                  Core.applicationArgument = (Typed.unTypedTerm arg0)})),
                Core.applicationArgument = (Typed.unTypedTerm arg1)})),
              Core.applicationArgument = (Typed.unTypedTerm arg2)})),
            Core.applicationArgument = (Typed.unTypedTerm arg3)})),
          Core.applicationArgument = (Typed.unTypedTerm arg4)})),
        Core.applicationArgument = (Typed.unTypedTerm arg5)})),
      Core.applicationArgument = (Typed.unTypedTerm arg6)}))

-- | DSL reference to hydra.analysis.gatherApplications
gatherApplications :: Typed.TypedTerm Core.Term -> Typed.TypedTerm ([Core.Term], Core.Term)
gatherApplications arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.gatherApplications")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.analysis.gatherArgs
gatherArgs :: Typed.TypedTerm Core.Term -> Typed.TypedTerm [Core.Term] -> Typed.TypedTerm (Core.Term, [Core.Term])
gatherArgs arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.gatherArgs")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.analysis.gatherArgsWithTypeApps
gatherArgsWithTypeApps :: Typed.TypedTerm Core.Term -> Typed.TypedTerm [Core.Term] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Core.Term, ([Core.Term], [Core.Type]))
gatherArgsWithTypeApps arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.gatherArgsWithTypeApps")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.analysis.isSelfTailRecursive
isSelfTailRecursive :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isSelfTailRecursive arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.isSelfTailRecursive")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.analysis.isSimpleAssignment
isSimpleAssignment :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isSimpleAssignment arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.isSimpleAssignment")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.analysis.isTailRecursiveInTailPosition
isTailRecursiveInTailPosition :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isTailRecursiveInTailPosition arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.isTailRecursiveInTailPosition")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.analysis.moduleContainsBinaryLiterals
moduleContainsBinaryLiterals :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm Bool
moduleContainsBinaryLiterals arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.moduleContainsBinaryLiterals")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.analysis.moduleContainsDecimalLiterals
moduleContainsDecimalLiterals :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm Bool
moduleContainsDecimalLiterals arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.moduleContainsDecimalLiterals")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.analysis.moduleDependencyModuleNames
moduleDependencyModuleNames :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Packaging.Module -> Typed.TypedTerm (Either Errors.Error (S.Set Packaging.ModuleName))
moduleDependencyModuleNames arg0 arg1 arg2 arg3 arg4 arg5 arg6 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.moduleDependencyModuleNames")),
                  Core.applicationArgument = (Typed.unTypedTerm arg0)})),
                Core.applicationArgument = (Typed.unTypedTerm arg1)})),
              Core.applicationArgument = (Typed.unTypedTerm arg2)})),
            Core.applicationArgument = (Typed.unTypedTerm arg3)})),
          Core.applicationArgument = (Typed.unTypedTerm arg4)})),
        Core.applicationArgument = (Typed.unTypedTerm arg5)})),
      Core.applicationArgument = (Typed.unTypedTerm arg6)}))

-- | DSL reference to hydra.analysis.moduleNamesForDefinitions
moduleNamesForDefinitions :: Typed.TypedTerm (Packaging.ModuleName -> t0) -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm (Util.ModuleNames t0)
moduleNamesForDefinitions arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.analysis.moduleNamesForDefinitions")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))
