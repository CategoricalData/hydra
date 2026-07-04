-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.constants

module Hydra.Dsl.Constants where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
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

-- | DSL reference to hydra.constants.debugInference
debugInference :: Typed.TypedTerm Bool
debugInference = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.debugInference"))

-- | DSL reference to hydra.constants.ignoredVariable
ignoredVariable :: Typed.TypedTerm String
ignoredVariable = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.ignoredVariable"))

-- | DSL reference to hydra.constants.keyClasses
keyClasses :: Typed.TypedTerm Core.Name
keyClasses = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyClasses"))

-- | DSL reference to hydra.constants.keyDebugId
keyDebugId :: Typed.TypedTerm Core.Name
keyDebugId = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyDebugId"))

-- | DSL reference to hydra.constants.keyDeprecated
keyDeprecated :: Typed.TypedTerm Core.Name
keyDeprecated = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyDeprecated"))

-- | DSL reference to hydra.constants.keyDescription
keyDescription :: Typed.TypedTerm Core.Name
keyDescription = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyDescription"))

-- | DSL reference to hydra.constants.keyExclude
keyExclude :: Typed.TypedTerm Core.Name
keyExclude = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyExclude"))

-- | DSL reference to hydra.constants.keyFirstClassType
keyFirstClassType :: Typed.TypedTerm Core.Name
keyFirstClassType = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyFirstClassType"))

-- | DSL reference to hydra.constants.keyFreshTypeVariableCount
keyFreshTypeVariableCount :: Typed.TypedTerm Core.Name
keyFreshTypeVariableCount = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyFreshTypeVariableCount"))

-- | DSL reference to hydra.constants.keyMaxLength
keyMaxLength :: Typed.TypedTerm Core.Name
keyMaxLength = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyMaxLength"))

-- | DSL reference to hydra.constants.keyMinLength
keyMinLength :: Typed.TypedTerm Core.Name
keyMinLength = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyMinLength"))

-- | DSL reference to hydra.constants.keyPreserveFieldName
keyPreserveFieldName :: Typed.TypedTerm Core.Name
keyPreserveFieldName = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyPreserveFieldName"))

-- | DSL reference to hydra.constants.keyType
keyType :: Typed.TypedTerm Core.Name
keyType = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.keyType"))

-- | DSL reference to hydra.constants.maxInt32
maxInt32 :: Typed.TypedTerm Int
maxInt32 = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.maxInt32"))

-- | DSL reference to hydra.constants.maxTraceDepth
maxTraceDepth :: Typed.TypedTerm Int
maxTraceDepth = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.maxTraceDepth"))

-- | DSL reference to hydra.constants.regexCamelCase
regexCamelCase :: Typed.TypedTerm String
regexCamelCase = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.regexCamelCase"))

-- | DSL reference to hydra.constants.regexNamespace
regexNamespace :: Typed.TypedTerm String
regexNamespace = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.regexNamespace"))

-- | DSL reference to hydra.constants.regexPackageName
regexPackageName :: Typed.TypedTerm String
regexPackageName = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.regexPackageName"))

-- | DSL reference to hydra.constants.regexPascalCase
regexPascalCase :: Typed.TypedTerm String
regexPascalCase = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.regexPascalCase"))

-- | DSL reference to hydra.constants.warningAutoGeneratedFile
warningAutoGeneratedFile :: Typed.TypedTerm String
warningAutoGeneratedFile = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.constants.warningAutoGeneratedFile"))
