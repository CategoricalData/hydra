module Hydra.Sources.Gql.PathAlgebra.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "com.gdblab.pathAlgebra.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A syntax model for the path algebra grammar by Angles et al."
      ++ " See the paper \"Path-based Algebraic Foundations of Graph Query Languages\""
      ++ " and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser"))}
  where
    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness).
    definitions = [
      alternation,
      boolOp,
      compareSym,
      complexCondition,
      complexFunction,
      compoundComplexCondition,
      concatenation,
      condition,
      edgeDirection,
      edgePattern,
      function_,
      groupBy,
      groupByOption,
      groupProj,
      label,
      nestedFunction,
      nodePattern,
      number,
      orderBy,
      orderByOption,
      partProj,
      pathName_,
      pathPattern,
      pathProj,
      pathQuery,
      plus_,
      projection,
      restrictorExt,
      rpq,
      rpqRestrictor,
      simpleFunction,
      star_,
      text,
      variable]

alternation :: TypeDefinition
alternation = define "Alternation" $
  doc "An RPQ alternation: either of two sub-expressions" $
  T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

boolOp :: TypeDefinition
boolOp = define "BoolOp" $
  doc "A boolean operator combining complex conditions: and or or" $
  T.enum [
  "and",
  "or"]

compareSym :: TypeDefinition
compareSym = define "CompareSym" $
  doc "A comparison symbol used in a path algebra condition" $
  T.enum [
  "equal",
  "notEqual",
  "lessThan",
  "greaterThan",
  "lessThanOrEqual",
  "greaterThanOrEqual"]

complexCondition :: TypeDefinition
complexCondition = define "ComplexCondition" $
  doc "A complex condition: a simple condition or a compound of conditions" $
  T.union [
  "simple">: pathAlg "Condition",
  "compound">: pathAlg "CompoundComplexCondition"]

complexFunction :: TypeDefinition
complexFunction = define "ComplexFunction" $
  doc "A complex path algebra function taking a name, an inner function, and an additional argument" $
  T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function",
  "additionalArg">: pathAlg "Text"]

compoundComplexCondition :: TypeDefinition
compoundComplexCondition = define "CompoundComplexCondition" $
  doc "A compound complex condition combining a simple condition and another complex condition with a boolean operator" $
  T.record [
  "lhs">: pathAlg "Condition",
  "operator">: pathAlg "BoolOp",
  "rhs">: pathAlg "ComplexCondition"]

concatenation :: TypeDefinition
concatenation = define "Concatenation" $
  doc "An RPQ concatenation: two sub-expressions in sequence" $
  T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

condition :: TypeDefinition
condition = define "Condition" $
  doc "A path algebra condition: a function, a comparison symbol, and a value" $
  T.record [
  "function">: pathAlg "Function",
  "compareSym">: pathAlg "CompareSym",
  "value">: pathAlg "Text"]

edgeDirection :: TypeDefinition
edgeDirection = define "EdgeDirection" $
  doc "The direction of an edge pattern: outgoing, incoming, or undirected" $
  T.enum [
  "outgoing",
  "incoming",
  "undirected"]

edgePattern :: TypeDefinition
edgePattern = define "EdgePattern" $
  doc "A pattern matching an edge, with a direction and an optional regular path query" $
  T.record [
  "direction">: pathAlg "EdgeDirection",
  "rpq">: T.optional $ pathAlg "Rpq"]

function_ :: TypeDefinition
function_ = define "Function" $
  doc "A path algebra function: simple, nested, or complex" $
  T.union [
  "simple">: pathAlg "SimpleFunction",
  "nested">: pathAlg "NestedFunction",
  "complex">: pathAlg "ComplexFunction"]

groupBy :: TypeDefinition
groupBy = define "GroupBy" $
  doc "A group-by clause, wrapping a group-by option" $
  T.wrap $ pathAlg "GroupByOption"

groupByOption :: TypeDefinition
groupByOption = define "GroupByOption" $
  doc "The grouping dimension of a group-by clause" $
  T.enum [
  "source",
  "target",
  "length",
  "sourceTarget",
  "sourceLength",
  "targetLength",
  "sourceTargetLength"]

groupProj :: TypeDefinition
groupProj = define "GroupProj" $
  doc "The group-count projection of a path query: all groups, or a limited number" $
  T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

label :: TypeDefinition
label = define "Label" $
  doc "An edge label" $
  T.string

nestedFunction :: TypeDefinition
nestedFunction = define "NestedFunction" $
  doc "A nested path algebra function taking a name and an inner function" $
  T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function"]

nodePattern :: TypeDefinition
nodePattern = define "NodePattern" $
  doc "A pattern matching a node, with an optional binding variable" $
  T.record [
  "variable">: T.optional $ pathAlg "Variable"]

-- Terminals from the grammar
number :: TypeDefinition
number = define "Number" $
  doc "A numeric literal" $
  T.bigint

orderBy :: TypeDefinition
orderBy = define "OrderBy" $
  doc "An order-by clause, wrapping an order-by option" $
  T.wrap $ pathAlg "OrderByOption"

orderByOption :: TypeDefinition
orderByOption = define "OrderByOption" $
  doc "The ordering dimension of an order-by clause" $
  T.enum [
  "partition",
  "group",
  "path",
  "partitionGroup",
  "partitionPath",
  "groupPath",
  "partitionGroupPath"]

partProj :: TypeDefinition
partProj = define "PartProj" $
  doc "The path-count projection of a path query: all paths, or a limited number" $
  T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

pathAlg :: String -> Type
pathAlg = typeref ns

pathName_ :: TypeDefinition
pathName_ = define "PathName" $
  doc "The name bound to a matched path" $
  T.string

pathPattern :: TypeDefinition
pathPattern = define "PathPattern" $
  doc "A pattern matching a path: a name, start node, edge, end node, and an optional condition" $
  T.record [
  "pathName">: pathAlg "PathName",
  "startNode">: pathAlg "NodePattern",
  "edge">: pathAlg "EdgePattern",
  "endNode">: pathAlg "NodePattern",
  "condition">: T.optional $ pathAlg "ComplexCondition"]

pathProj :: TypeDefinition
pathProj = define "PathProj" $
  doc "The path projection of a path query: all paths, or a limited number" $
  T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

-- Nonterminal productions from the grammar
pathQuery :: TypeDefinition
pathQuery = define "PathQuery" $
  doc "A path algebra query: a projection, optional restrictor, path pattern, and optional grouping/ordering" $
  T.record [
  "projection">: pathAlg "Projection",
  "restrictorExt">: T.optional $ pathAlg "RestrictorExt",
  "pathPattern">: pathAlg "PathPattern",
  "groupBy">: T.optional $ pathAlg "GroupBy",
  "orderBy">: T.optional $ pathAlg "OrderBy"]

plus_ :: TypeDefinition
plus_ = define "Plus" $
  doc "An RPQ one-or-more repetition of a sub-expression, with an optional restrictor" $
  T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

projection :: TypeDefinition
projection = define "Projection" $
  doc "The projection of a path query: part, group, and path projections" $
  T.record [
  "partProj">: pathAlg "PartProj",
  "groupProj">: pathAlg "GroupProj",
  "pathProj">: pathAlg "PathProj"]

restrictorExt :: TypeDefinition
restrictorExt = define "RestrictorExt" $
  doc "A path restrictor: walk, trail, simple, acyclic, or shortest" $
  T.enum [
  "walk",
  "trail",
  "simple",
  "acyclic",
  "shortest"]

rpq :: TypeDefinition
rpq = define "Rpq" $
  doc "A regular path query expression" $
  T.union [
  "parenthesis">: pathAlg "Rpq",
  "label">: pathAlg "Label",
  "negated">: pathAlg "Label",
  "reverse">: pathAlg "Label",
  "optional">: pathAlg "Rpq",
  "plus">: pathAlg "Plus",
  "star">: pathAlg "Star",
  "concatenation">: pathAlg "Concatenation",
  "alternation">: pathAlg "Alternation"]

rpqRestrictor :: TypeDefinition
rpqRestrictor = define "RpqRestrictor" $
  doc "A restrictor applied to a regular path query, wrapping a restrictor extension" $
  T.wrap $ pathAlg "RestrictorExt"

simpleFunction :: TypeDefinition
simpleFunction = define "SimpleFunction" $
  doc "A simple path algebra function taking a name and a single argument" $
  T.record [
  "name">: pathAlg "Text",
  "argument">: pathAlg "Text"]

star_ :: TypeDefinition
star_ = define "Star" $
  doc "An RPQ zero-or-more repetition of a sub-expression, with an optional restrictor" $
  T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

text :: TypeDefinition
text = define "Text" $
  doc "A text literal" $
  T.string

variable :: TypeDefinition
variable = define "Variable" $
  doc "A variable name bound in a path pattern" $
  T.string
