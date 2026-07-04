-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.json.parser

module Hydra.Dsl.Json.Parser where

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
import qualified Hydra.Json.Parser as Parser
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsers as Parsers
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

-- | DSL reference to hydra.json.parser.digit
digit :: Typed.TypedTerm (Parsing.Parser Int)
digit = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.digit"))

-- | DSL reference to hydra.json.parser.digits
digits :: Typed.TypedTerm (Parsing.Parser String)
digits = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.digits"))

-- | DSL reference to hydra.json.parser.jsonArray
jsonArray :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonArray = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonArray"))

-- | DSL reference to hydra.json.parser.jsonBool
jsonBool :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonBool = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonBool"))

-- | DSL reference to hydra.json.parser.jsonEscapeChar
jsonEscapeChar :: Typed.TypedTerm (Parsing.Parser Int)
jsonEscapeChar = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonEscapeChar"))

-- | DSL reference to hydra.json.parser.jsonExponentPart
jsonExponentPart :: Typed.TypedTerm (Parsing.Parser (Maybe String))
jsonExponentPart = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonExponentPart"))

-- | DSL reference to hydra.json.parser.jsonFractionPart
jsonFractionPart :: Typed.TypedTerm (Parsing.Parser (Maybe String))
jsonFractionPart = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonFractionPart"))

-- | DSL reference to hydra.json.parser.jsonIntegerPart
jsonIntegerPart :: Typed.TypedTerm (Parsing.Parser String)
jsonIntegerPart = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonIntegerPart"))

-- | DSL reference to hydra.json.parser.jsonKeyValue
jsonKeyValue :: Typed.TypedTerm (Parsing.Parser (String, Model.Value))
jsonKeyValue = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonKeyValue"))

-- | DSL reference to hydra.json.parser.jsonNull
jsonNull :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonNull = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonNull"))

-- | DSL reference to hydra.json.parser.jsonNumber
jsonNumber :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonNumber = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonNumber"))

-- | DSL reference to hydra.json.parser.jsonObject
jsonObject :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonObject = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonObject"))

-- | DSL reference to hydra.json.parser.jsonString
jsonString :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonString = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonString"))

-- | DSL reference to hydra.json.parser.jsonStringChar
jsonStringChar :: Typed.TypedTerm (Parsing.Parser Int)
jsonStringChar = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonStringChar"))

-- | DSL reference to hydra.json.parser.jsonValue
jsonValue :: Typed.TypedTerm (Parsing.Parser Model.Value)
jsonValue = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.jsonValue"))

-- | DSL reference to hydra.json.parser.parseJson
parseJson :: Typed.TypedTerm String -> Typed.TypedTerm (Parsing.ParseResult Model.Value)
parseJson arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.json.parser.parseJson")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.json.parser.token
token :: Typed.TypedTerm (Parsing.Parser t0) -> Typed.TypedTerm (Parsing.Parser t0)
token arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.json.parser.token")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.json.parser.whitespace
whitespace :: Typed.TypedTerm (Parsing.Parser ())
whitespace = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.json.parser.whitespace"))
