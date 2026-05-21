module Hydra.Sources.Show.Error.Pg where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))

-- Additional imports
import           Hydra.Error.Pg
import qualified Hydra.Sources.Error.Pg                    as ErrorPg
import qualified Hydra.Pg.Model                            as PG


ns :: Namespace
ns = Namespace "hydra.show.error.pg"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ((ErrorPg.ns:KernelTypes.kernelTypesNamespaces)),
            moduleDescription = Just "String representations of hydra.error.pg types"}
  where
    definitions = [
      toDefinition invalidEdgeError,
      toDefinition invalidElementPropertyError,
      toDefinition invalidGraphEdgeError,
      toDefinition invalidGraphError,
      toDefinition invalidGraphVertexError,
      toDefinition invalidPropertyError,
      toDefinition invalidValueError,
      toDefinition invalidVertexError,
      toDefinition noSuchEdgeLabelError,
      toDefinition noSuchVertexLabelError,
      toDefinition wrongVertexLabelError]

invalidValueError :: TTermDefinition (InvalidValueError -> String)
invalidValueError = define "invalidValueError" $
  doc "Show an invalid value error as a string" $
  "e" ~> Strings.cat $ list [
    string "expected ",
    project _InvalidValueError _InvalidValueError_expectedType @@ var "e",
    string ", got ",
    project _InvalidValueError _InvalidValueError_value @@ var "e"]

noSuchEdgeLabelError :: TTermDefinition (NoSuchEdgeLabelError -> String)
noSuchEdgeLabelError = define "noSuchEdgeLabelError" $
  doc "Show a no-such-edge-label error as a string" $
  "e" ~> Strings.cat $ list [
    string "no such edge label: ",
    unwrap PG._EdgeLabel @@ (project _NoSuchEdgeLabelError _NoSuchEdgeLabelError_label @@ var "e")]

noSuchVertexLabelError :: TTermDefinition (NoSuchVertexLabelError -> String)
noSuchVertexLabelError = define "noSuchVertexLabelError" $
  doc "Show a no-such-vertex-label error as a string" $
  "e" ~> Strings.cat $ list [
    string "no such vertex label: ",
    unwrap PG._VertexLabel @@ (project _NoSuchVertexLabelError _NoSuchVertexLabelError_label @@ var "e")]

wrongVertexLabelError :: TTermDefinition (WrongVertexLabelError -> String)
wrongVertexLabelError = define "wrongVertexLabelError" $
  doc "Show a wrong-vertex-label error as a string" $
  "e" ~> Strings.cat $ list [
    string "expected vertex label ",
    unwrap PG._VertexLabel @@ (project _WrongVertexLabelError _WrongVertexLabelError_expected @@ var "e"),
    string ", got ",
    unwrap PG._VertexLabel @@ (project _WrongVertexLabelError _WrongVertexLabelError_actual @@ var "e")]

invalidPropertyError :: TTermDefinition (InvalidPropertyError -> String)
invalidPropertyError = define "invalidPropertyError" $
  doc "Show an invalid property error as a string" $
  "e" ~>
  cases _InvalidPropertyError (var "e") Nothing [
    _InvalidPropertyError_invalidValue>>:
      ("v" ~> Strings.cat2 (string "invalid value: ") (invalidValueError @@ var "v")),
    _InvalidPropertyError_missingRequired>>:
      ("k" ~> Strings.cat2 (string "missing required property: ") (unwrap PG._PropertyKey @@ var "k")),
    _InvalidPropertyError_unexpectedKey>>:
      ("k" ~> Strings.cat2 (string "unexpected property key: ") (unwrap PG._PropertyKey @@ var "k"))]

invalidElementPropertyError :: TTermDefinition (InvalidElementPropertyError -> String)
invalidElementPropertyError = define "invalidElementPropertyError" $
  doc "Show an invalid element property error as a string" $
  "e" ~> Strings.cat $ list [
    string "property ",
    unwrap PG._PropertyKey @@ (project _InvalidElementPropertyError _InvalidElementPropertyError_key @@ var "e"),
    string ": ",
    invalidPropertyError @@ (project _InvalidElementPropertyError _InvalidElementPropertyError_error @@ var "e")]

invalidVertexError :: TTermDefinition (InvalidVertexError -> String)
invalidVertexError = define "invalidVertexError" $
  doc "Show an invalid vertex error as a string" $
  "e" ~>
  cases _InvalidVertexError (var "e") Nothing [
    _InvalidVertexError_id>>:
      ("v" ~> Strings.cat2 (string "invalid id: ") (invalidValueError @@ var "v")),
    _InvalidVertexError_label>>:
      ("l" ~> noSuchVertexLabelError @@ var "l"),
    _InvalidVertexError_property>>:
      ("p" ~> invalidElementPropertyError @@ var "p")]

invalidEdgeError :: TTermDefinition (InvalidEdgeError -> String)
invalidEdgeError = define "invalidEdgeError" $
  doc "Show an invalid edge error as a string" $
  "e" ~>
  cases _InvalidEdgeError (var "e") Nothing [
    _InvalidEdgeError_id>>:
      ("v" ~> Strings.cat2 (string "invalid id: ") (invalidValueError @@ var "v")),
    _InvalidEdgeError_inVertexLabel>>:
      ("w" ~> Strings.cat2 (string "wrong in-vertex label: ") (wrongVertexLabelError @@ var "w")),
    _InvalidEdgeError_inVertexNotFound>>:
      ("_" ~> string "in-vertex not found"),
    _InvalidEdgeError_label>>:
      ("l" ~> noSuchEdgeLabelError @@ var "l"),
    _InvalidEdgeError_outVertexLabel>>:
      ("w" ~> Strings.cat2 (string "wrong out-vertex label: ") (wrongVertexLabelError @@ var "w")),
    _InvalidEdgeError_outVertexNotFound>>:
      ("_" ~> string "out-vertex not found"),
    _InvalidEdgeError_property>>:
      ("p" ~> invalidElementPropertyError @@ var "p")]

invalidGraphVertexError :: TTermDefinition ((v -> String) -> InvalidGraphVertexError v -> String)
invalidGraphVertexError = define "invalidGraphVertexError" $
  doc "Show an invalid graph vertex error as a string, given a value printer" $
  "printValue" ~> "e" ~> Strings.cat $ list [
    string "vertex ",
    var "printValue" @@ (project _InvalidGraphVertexError _InvalidGraphVertexError_id @@ var "e"),
    string ": ",
    invalidVertexError @@ (project _InvalidGraphVertexError _InvalidGraphVertexError_error @@ var "e")]

invalidGraphEdgeError :: TTermDefinition ((v -> String) -> InvalidGraphEdgeError v -> String)
invalidGraphEdgeError = define "invalidGraphEdgeError" $
  doc "Show an invalid graph edge error as a string, given a value printer" $
  "printValue" ~> "e" ~> Strings.cat $ list [
    string "edge ",
    var "printValue" @@ (project _InvalidGraphEdgeError _InvalidGraphEdgeError_id @@ var "e"),
    string ": ",
    invalidEdgeError @@ (project _InvalidGraphEdgeError _InvalidGraphEdgeError_error @@ var "e")]

invalidGraphError :: TTermDefinition ((v -> String) -> InvalidGraphError v -> String)
invalidGraphError = define "invalidGraphError" $
  doc "Show an invalid graph error as a string, given a value printer" $
  "printValue" ~> "e" ~>
  cases _InvalidGraphError (var "e") Nothing [
    _InvalidGraphError_edge>>:
      ("ge" ~> invalidGraphEdgeError @@ var "printValue" @@ var "ge"),
    _InvalidGraphError_vertex>>:
      ("gv" ~> invalidGraphVertexError @@ var "printValue" @@ var "gv")]
