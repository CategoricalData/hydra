-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.show.docs

module Hydra.Dsl.Show.Docs where

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
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Read.Docs as ReadDocs
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Docs as ShowDocs
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

-- | DSL reference to hydra.show.docs.definitionReference
definitionReference :: Typed.TypedTerm Packaging.DefinitionReference -> Typed.TypedTerm String
definitionReference arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.definitionReference")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.show.docs.docSegment
docSegment :: Typed.TypedTerm Docs.DocSegment -> Typed.TypedTerm String
docSegment arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.docSegment")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.show.docs.docSegmentWith
docSegmentWith :: Typed.TypedTerm (Packaging.EntityReference -> String) -> Typed.TypedTerm Docs.DocSegment -> Typed.TypedTerm String
docSegmentWith arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.docSegmentWith")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.show.docs.docSegments
docSegments :: Typed.TypedTerm [Docs.DocSegment] -> Typed.TypedTerm String
docSegments arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.docSegments")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.show.docs.docSegmentsWith
docSegmentsWith :: Typed.TypedTerm (Packaging.EntityReference -> String) -> Typed.TypedTerm [Docs.DocSegment] -> Typed.TypedTerm String
docSegmentsWith arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.docSegmentsWith")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.show.docs.entityReference
entityReference :: Typed.TypedTerm Packaging.EntityReference -> Typed.TypedTerm String
entityReference arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.entityReference")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.show.docs.renderDocString
renderDocString :: Typed.TypedTerm String -> Typed.TypedTerm String
renderDocString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.renderDocString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.show.docs.renderDocStringWith
renderDocStringWith :: Typed.TypedTerm (Packaging.EntityReference -> String) -> Typed.TypedTerm String -> Typed.TypedTerm String
renderDocStringWith arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.show.docs.renderDocStringWith")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
