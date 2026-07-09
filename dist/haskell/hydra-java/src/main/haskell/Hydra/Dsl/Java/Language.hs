-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.java.language

module Hydra.Dsl.Java.Language where

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
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Lexical as DslLexical
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
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
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Language as Language
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
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
import qualified Data.Set as S

-- | DSL reference to hydra.java.language.javaLanguage
javaLanguage :: Typed.TypedTerm Coders.Language
javaLanguage = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.java.language.javaLanguage"))

-- | DSL reference to hydra.java.language.javaMaxTupleLength
javaMaxTupleLength :: Typed.TypedTerm Int
javaMaxTupleLength = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.java.language.javaMaxTupleLength"))

-- | DSL reference to hydra.java.language.reservedWords
reservedWords :: Typed.TypedTerm (S.Set String)
reservedWords = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.java.language.reservedWords"))
