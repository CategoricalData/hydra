-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.strip

module Hydra.Dsl.Strip where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
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
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Rewriting as DslRewriting
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
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
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
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
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL reference to hydra.strip.deannotateAndDetypeTerm
deannotateAndDetypeTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
deannotateAndDetypeTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateAndDetypeTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.deannotateTerm
deannotateTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
deannotateTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.deannotateType
deannotateType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
deannotateType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.deannotateTypeParameters
deannotateTypeParameters :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
deannotateTypeParameters arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateTypeParameters")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.deannotateTypeRecursive
deannotateTypeRecursive :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
deannotateTypeRecursive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateTypeRecursive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.deannotateTypeSchemeRecursive
deannotateTypeSchemeRecursive :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm Core.TypeScheme
deannotateTypeSchemeRecursive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.deannotateTypeSchemeRecursive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.detypeTerm
detypeTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
detypeTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.detypeTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.removeTermAnnotations
removeTermAnnotations :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
removeTermAnnotations arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.removeTermAnnotations")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.removeTypeAnnotations
removeTypeAnnotations :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
removeTypeAnnotations arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.removeTypeAnnotations")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.removeTypeAnnotationsFromTerm
removeTypeAnnotationsFromTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
removeTypeAnnotationsFromTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.removeTypeAnnotationsFromTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.removeTypesFromTerm
removeTypesFromTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
removeTypesFromTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.removeTypesFromTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.strip.stripTypeLambdas
stripTypeLambdas :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
stripTypeLambdas arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.strip.stripTypeLambdas")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
