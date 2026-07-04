-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.checking

module Hydra.Dsl.Checking where

import qualified Hydra.Ast as Ast
import qualified Hydra.Checking as Checking
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
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
import qualified Hydra.Dsl.Formatting as DslFormatting
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Lexical as DslLexical
import qualified Hydra.Dsl.Names as DslNames
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Resolution as DslResolution
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
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.File as File
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Relational as Relational
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Errors as ShowErrors
import qualified Hydra.Show.Variants as ShowVariants
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
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

-- | DSL reference to hydra.checking.allEqual
allEqual :: Typed.TypedTerm [t0] -> Typed.TypedTerm Bool
allEqual arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.allEqual")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.checking.applyTypeArgumentsToType
applyTypeArgumentsToType :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either Errors.Error Core.Type)
applyTypeArgumentsToType arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.applyTypeArgumentsToType")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.checkForUnboundTypeVariables
checkForUnboundTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error ())
checkForUnboundTypeVariables arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkForUnboundTypeVariables")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.checking.checkNominalApplication
checkNominalApplication :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Either Errors.Error ((), Typing.InferenceContext))
checkNominalApplication arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkNominalApplication")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.checkSameType
checkSameType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm String -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Either Errors.Error Core.Type)
checkSameType arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkSameType")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.checkType
checkType :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either Errors.Error ())
checkType arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkType")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.checkTypeSubst
checkTypeSubst :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm (Either Errors.Error Typing.TypeSubst)
checkTypeSubst arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkTypeSubst")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.checking.checkTypeVariables
checkTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm ()
checkTypeVariables arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.checkTypeVariables")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.checking.containsInScopeTypeVars
containsInScopeTypeVars :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
containsInScopeTypeVars arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.containsInScopeTypeVars")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.checking.normalizeTypeFreeVars
normalizeTypeFreeVars :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
normalizeTypeFreeVars arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.normalizeTypeFreeVars")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.checking.toFContext
toFContext :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.Type)
toFContext arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.toFContext")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.checking.typeListsEffectivelyEqual
typeListsEffectivelyEqual :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Bool
typeListsEffectivelyEqual arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeListsEffectivelyEqual")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.checking.typeOf
typeOf :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOf arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOf")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfAnnotatedTerm
typeOfAnnotatedTerm :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfAnnotatedTerm arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfAnnotatedTerm")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfApplication
typeOfApplication :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Application -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfApplication arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfApplication")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfCaseStatement
typeOfCaseStatement :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfCaseStatement arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfCaseStatement")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfEither
typeOfEither :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Either Core.Term Core.Term) -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfEither arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfEither")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfInjection
typeOfInjection :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Injection -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfInjection arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfInjection")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfLambda
typeOfLambda :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Lambda -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfLambda arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfLambda")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfLet
typeOfLet :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Let -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfLet arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfLet")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfList
typeOfList :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm [Core.Term] -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfList arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfList")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfLiteral
typeOfLiteral :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Literal -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfLiteral arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfLiteral")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfMap
typeOfMap :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (M.Map Core.Term Core.Term) -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfMap arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfMap")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfMaybe
typeOfMaybe :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfMaybe arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfMaybe")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfPair
typeOfPair :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Core.Term, Core.Term) -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfPair arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfPair")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfPrimitive
typeOfPrimitive :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfPrimitive arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfPrimitive")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfProjection
typeOfProjection :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Projection -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfProjection arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfProjection")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfRecord
typeOfRecord :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Record -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfRecord arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfRecord")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfSet
typeOfSet :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (S.Set Core.Term) -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfSet arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfSet")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfTerm
typeOfTerm :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error Core.Type)
typeOfTerm arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfTerm")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.checking.typeOfTypeApplication
typeOfTypeApplication :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfTypeApplication arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfTypeApplication")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfTypeLambda
typeOfTypeLambda :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfTypeLambda arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfTypeLambda")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfUnit
typeOfUnit :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfUnit arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfUnit")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.checking.typeOfUnwrap
typeOfUnwrap :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfUnwrap arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfUnwrap")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfVariable
typeOfVariable :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfVariable arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfVariable")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typeOfWrappedTerm
typeOfWrappedTerm :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm (Either Errors.Error (Core.Type, Typing.InferenceContext))
typeOfWrappedTerm arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typeOfWrappedTerm")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.checking.typesAllEffectivelyEqual
typesAllEffectivelyEqual :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Bool
typesAllEffectivelyEqual arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typesAllEffectivelyEqual")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.checking.typesEffectivelyEqual
typesEffectivelyEqual :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
typesEffectivelyEqual arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.checking.typesEffectivelyEqual")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))
