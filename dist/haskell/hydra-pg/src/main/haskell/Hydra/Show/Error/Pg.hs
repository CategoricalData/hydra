-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.error.pg types

module Hydra.Show.Error.Pg where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.Pg as Pg
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Pg.Model as PgModel
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
-- | Show an invalid edge error as a string
invalidEdgeError :: Pg.InvalidEdgeError -> String
invalidEdgeError e =
    case e of
      Pg.InvalidEdgeErrorId v0 -> Strings.cat2 "invalid id: " (invalidValueError v0)
      Pg.InvalidEdgeErrorInVertexLabel v0 -> Strings.cat2 "wrong in-vertex label: " (wrongVertexLabelError v0)
      Pg.InvalidEdgeErrorInVertexNotFound -> "in-vertex not found"
      Pg.InvalidEdgeErrorLabel v0 -> noSuchEdgeLabelError v0
      Pg.InvalidEdgeErrorOutVertexLabel v0 -> Strings.cat2 "wrong out-vertex label: " (wrongVertexLabelError v0)
      Pg.InvalidEdgeErrorOutVertexNotFound -> "out-vertex not found"
      Pg.InvalidEdgeErrorProperty v0 -> invalidElementPropertyError v0
-- | Show an invalid element property error as a string
invalidElementPropertyError :: Pg.InvalidElementPropertyError -> String
invalidElementPropertyError e =
    Strings.cat [
      "property ",
      (PgModel.unPropertyKey (Pg.invalidElementPropertyErrorKey e)),
      ": ",
      (invalidPropertyError (Pg.invalidElementPropertyErrorError e))]
-- | Show an invalid graph edge error as a string, given a value printer
invalidGraphEdgeError :: (t0 -> String) -> Pg.InvalidGraphEdgeError t0 -> String
invalidGraphEdgeError printValue e =
    Strings.cat [
      "edge ",
      (printValue (Pg.invalidGraphEdgeErrorId e)),
      ": ",
      (invalidEdgeError (Pg.invalidGraphEdgeErrorError e))]
-- | Show an invalid graph error as a string, given a value printer
invalidGraphError :: (t0 -> String) -> Pg.InvalidGraphError t0 -> String
invalidGraphError printValue e =
    (\x -> case x of
      Pg.InvalidGraphErrorEdge v0 -> invalidGraphEdgeError printValue v0
      Pg.InvalidGraphErrorVertex v0 -> invalidGraphVertexError printValue v0) e
-- | Show an invalid graph vertex error as a string, given a value printer
invalidGraphVertexError :: (t0 -> String) -> Pg.InvalidGraphVertexError t0 -> String
invalidGraphVertexError printValue e =
    Strings.cat [
      "vertex ",
      (printValue (Pg.invalidGraphVertexErrorId e)),
      ": ",
      (invalidVertexError (Pg.invalidGraphVertexErrorError e))]
-- | Show an invalid property error as a string
invalidPropertyError :: Pg.InvalidPropertyError -> String
invalidPropertyError e =
    case e of
      Pg.InvalidPropertyErrorInvalidValue v0 -> Strings.cat2 "invalid value: " (invalidValueError v0)
      Pg.InvalidPropertyErrorMissingRequired v0 -> Strings.cat2 "missing required property: " (PgModel.unPropertyKey v0)
      Pg.InvalidPropertyErrorUnexpectedKey v0 -> Strings.cat2 "unexpected property key: " (PgModel.unPropertyKey v0)
-- | Show an invalid value error as a string
invalidValueError :: Pg.InvalidValueError -> String
invalidValueError e =
    Strings.cat [
      "expected ",
      (Pg.invalidValueErrorExpectedType e),
      ", got ",
      (Pg.invalidValueErrorValue e)]
-- | Show an invalid vertex error as a string
invalidVertexError :: Pg.InvalidVertexError -> String
invalidVertexError e =
    case e of
      Pg.InvalidVertexErrorId v0 -> Strings.cat2 "invalid id: " (invalidValueError v0)
      Pg.InvalidVertexErrorLabel v0 -> noSuchVertexLabelError v0
      Pg.InvalidVertexErrorProperty v0 -> invalidElementPropertyError v0
-- | Show a no-such-edge-label error as a string
noSuchEdgeLabelError :: Pg.NoSuchEdgeLabelError -> String
noSuchEdgeLabelError e =
    Strings.cat [
      "no such edge label: ",
      (PgModel.unEdgeLabel (Pg.noSuchEdgeLabelErrorLabel e))]
-- | Show a no-such-vertex-label error as a string
noSuchVertexLabelError :: Pg.NoSuchVertexLabelError -> String
noSuchVertexLabelError e =
    Strings.cat [
      "no such vertex label: ",
      (PgModel.unVertexLabel (Pg.noSuchVertexLabelErrorLabel e))]
-- | Show a wrong-vertex-label error as a string
wrongVertexLabelError :: Pg.WrongVertexLabelError -> String
wrongVertexLabelError e =
    Strings.cat [
      "expected vertex label ",
      (PgModel.unVertexLabel (Pg.wrongVertexLabelErrorExpected e)),
      ", got ",
      (PgModel.unVertexLabel (Pg.wrongVertexLabelErrorActual e))]
