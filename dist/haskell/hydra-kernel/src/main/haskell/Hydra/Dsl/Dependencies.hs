-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.dependencies

module Hydra.Dsl.Dependencies where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Annotations as DslAnnotations
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Constants as DslConstants
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
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
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Rewriting as DslRewriting
import qualified Hydra.Dsl.Sorting as DslSorting
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
import qualified Hydra.Error.Checking as Checking
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
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Sorting as Sorting
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
import qualified Data.Map as M
import qualified Data.Set as S

-- | DSL reference to hydra.dependencies.definitionsWithDependencies
definitionsWithDependencies :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (Either Errors.Error [Core.Binding])
definitionsWithDependencies arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.definitionsWithDependencies")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.dependencies.flattenLetTerms
flattenLetTerms :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
flattenLetTerms arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.flattenLetTerms")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.inlineType
inlineType :: Typed.TypedTerm (M.Map Core.Name Core.Type) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either Errors.Error Core.Type)
inlineType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.inlineType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.dependencies.isLambda
isLambda :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isLambda arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.isLambda")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.liftLambdaAboveLet
liftLambdaAboveLet :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
liftLambdaAboveLet arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.liftLambdaAboveLet")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.pruneLet
pruneLet :: Typed.TypedTerm Core.Let -> Typed.TypedTerm Core.Let
pruneLet arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.pruneLet")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.replaceTypedefs
replaceTypedefs :: Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
replaceTypedefs arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.replaceTypedefs")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.dependencies.simplifyTerm
simplifyTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
simplifyTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.simplifyTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.termDependencyNames
termDependencyNames :: Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (S.Set Core.Name)
termDependencyNames arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.termDependencyNames")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.dependencies.toShortNames
toShortNames :: Typed.TypedTerm [Core.Name] -> Typed.TypedTerm (M.Map Core.Name Core.Name)
toShortNames arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.toShortNames")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.topologicalSortBindingMap
topologicalSortBindingMap :: Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm [[(Core.Name, Core.Term)]]
topologicalSortBindingMap arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.topologicalSortBindingMap")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.topologicalSortBindings
topologicalSortBindings :: Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (Either [[Core.Name]] [Core.Name])
topologicalSortBindings arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.topologicalSortBindings")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.topologicalSortTypeDefinitions
topologicalSortTypeDefinitions :: Typed.TypedTerm [Packaging.TypeDefinition] -> Typed.TypedTerm [[Packaging.TypeDefinition]]
topologicalSortTypeDefinitions arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.topologicalSortTypeDefinitions")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.dependencies.typeDependencyNames
typeDependencyNames :: Typed.TypedTerm Bool -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (S.Set Core.Name)
typeDependencyNames arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.typeDependencyNames")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.dependencies.typeNamesInType
typeNamesInType :: Ord t0 => (Typed.TypedTerm Core.Type -> Typed.TypedTerm (S.Set t0))
typeNamesInType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.dependencies.typeNamesInType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
