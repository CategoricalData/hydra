module Hydra.Sources.Kernel.Types.Query where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.query"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "A model for language-agnostic graph pattern queries"
  where
    elements = [
      comparisonConstraint,
      edge,
      graphPattern,
      node,
      path,
      pattern,
      query,
      range,
      regexQuantifier,
      regexSequence,
      step,
      triplePattern,
      variable]

comparisonConstraint :: Binding
comparisonConstraint = define "ComparisonConstraint" $
  doc "One of several comparison operators" $
  T.enum ["equal", "notEqual", "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"]

edge :: Binding
edge = define "Edge" $
  doc "An abstract edge based on a record type" $
  T.record [
    "type">:
      doc "The name of a record type, for which the edge also specifies an out- and an in- projection"
      Core.name,
    "out">:
      doc "The field representing the out-projection of the edge. Defaults to 'out'." $
      T.optional Core.name,
    "in">:
      doc "The field representing the in-projection of the edge. Defaults to 'in'." $
      T.optional Core.name]

graphPattern :: Binding
graphPattern = define "GraphPattern" $
  doc "A query pattern which matches within a designated component subgraph" $
  T.record [
    "graph">:
      doc "The name of the component graph"
      Core.name,
    "patterns">:
      doc "The patterns to match within the subgraph" $
      T.list pattern]

node :: Binding
node = define "Node" $
  doc "A node in a query expression; it may be a term, a variable, or a wildcard" $
  T.union [
    "term">:
      doc "A graph term; an expression which is valid in the graph being matched"
      Core.term,
    "variable">:
      doc "A query variable, not to be confused with a variable term"
      variable,
    "wildcard">:
      doc "An anonymous variable which we do not care to join across patterns" T.unit]

path :: Binding
path = define "Path" $
  doc "A query path" $
  T.union [
    "step">:
      doc "A path given by a single step"
      step,
    "regex">:
      doc "A path given by a regular expression quantifier applied to another path"
      regexSequence,
    "inverse">:
      doc "A path given by the inverse of another path"
      path]

pattern :: Binding
pattern = define "Pattern" $
  doc "A query pattern" $
  T.union [
    "triple">:
      doc "A subject/predicate/object pattern"
      triplePattern,
    "negation">:
      doc "The negation of another pattern"
      pattern,
    "conjunction">:
      doc "The conjunction ('and') of several other patterns" $
      T.list pattern,
    "disjunction">:
      doc "The disjunction (inclusive 'or') of several other patterns" $
      T.list pattern,
    "graph">:
      doc "A pattern which matches within a named subgraph"
      graphPattern]

query :: Binding
query = define "Query" $
  doc "A SELECT-style graph pattern matching query" $
  T.record [
    "variables">:
      doc "The variables selected by the query" $
      T.list variable,
    "patterns">:
      doc "The patterns to be matched" $
      T.list pattern]

range :: Binding
range = define "Range" $
  doc "A range from min to max, inclusive" $
  T.record [
    "min">:
      doc "The minimum value (inclusive)" $
      T.int32,
    "max">:
      doc "The maximum value (inclusive)" $
      T.int32]

regexQuantifier :: Binding
regexQuantifier = define "RegexQuantifier" $
  doc "A regular expression quantifier" $
  T.union [
    "one">: doc "No quantifier; matches a single occurrence" T.unit,
    "zeroOrOne">: doc "The ? quanifier; matches zero or one occurrence" T.unit,
    "zeroOrMore">: doc "The * quantifier; matches any number of occurrences" T.unit,
    "oneOrMore">: doc "The + quantifier; matches one or more occurrences" T.unit,
    "exactly">: doc "The {n} quantifier; matches exactly n occurrences" T.int32,
    "atLeast">: doc "The {n,} quantifier; matches at least n occurrences" T.int32,
    "range">: doc "The {n, m} quantifier; matches between n and m (inclusive) occurrences" range]

regexSequence :: Binding
regexSequence = define "RegexSequence" $
  doc "A path with a regex quantifier" $
  T.record [
    "path">:
      doc "The path to which the quantifier applies"
      path,
    "quantifier">:
      doc "The quantifier"
      regexQuantifier]

step :: Binding
step = define "Step" $
  doc "An atomic function as part of a query. When applied to a graph, steps are typed by function types." $
  T.union [
    "edge">:
      doc "An out-to-in traversal of an abstract edge"
      edge,
    "project">:
      doc "A projection from a record through one of its fields"
      Core.projection,
    "compare">:
      doc "A comparison of two terms"
      comparisonConstraint]

triplePattern :: Binding
triplePattern = define "TriplePattern" $
  doc "A subject/predicate/object pattern" $
  T.record [
    "subject">:
      doc "The subject of the pattern"
      node,
    "predicate">:
      doc "The predicate (property) of the pattern"
      path,
    "object">:
      doc "The object of the pattern"
      node]

variable :: Binding
variable = define "Variable" $
  doc "A query variable" $
  T.wrap T.string
