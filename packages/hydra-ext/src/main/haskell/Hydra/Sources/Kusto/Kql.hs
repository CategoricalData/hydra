module Hydra.Sources.Kusto.Kql where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.kusto.kql"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just ("A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.")}
  where
    definitions = [
      betweenExpression,
      binaryExpression,
      binaryOperator,
      builtInFunction,
      columnAlias,
      columnAssignment,
      columnName,
      columns,
      command,
      datetime,
      duration,
      durationUnit,
      expression,
      function_,
      functionExpression,
      functionName,
      indexExpression,
      joinCommand,
      joinKind,
      keyValuePair,
      letBinding,
      letExpression,
      literal_,
      order,
      parameter,
      parseCommand,
      pipelineExpression,
      printCommand,
      projection,
      propertyExpression,
      query,
      searchCommand,
      summarizeCommand,
      tableName,
      topCommand,
      sortBy,
      tabularExpression,
      unaryExpression,
      unaryOperator,
      unionCommand,
      unionKind]

betweenExpression :: TypeDefinition
betweenExpression = define "BetweenExpression" $
  T.record [
    "not">: T.boolean,
    "expression">: kql "Expression",
    "lowerBound">: kql "Expression",
    "upperBound">: kql "Expression"]

binaryExpression :: TypeDefinition
binaryExpression = define "BinaryExpression" $
  T.record [
    "left">: kql "Expression",
    "operator">: kql "BinaryOperator",
    "right">: kql "Expression"]

binaryOperator :: TypeDefinition
binaryOperator = define "BinaryOperator" $
  T.enum [
    "caseInsensitiveEqual",
    "contains",
    "divide",
    "endsWith",
    "equal",
    "greater",
    "greaterOrEqual",
    "has",
    "hasPrefix",
    "hasSuffix",
    "less",
    "lessOrEqual",
    "matchesRegex",
    "minus",
    "notEqual",
    "plus",
    "startsWith",
    "times"]

builtInFunction :: TypeDefinition
builtInFunction = define "BuiltInFunction" $
  T.enum [
    "ago",
    "bin",
    "count",
    "dcount",
    "endofday",
    "extract",
    "format_datetime",
    "materialize",
    "now",
    "range",
    "startofday",
    "strcat",
    "todynamic"]

columnAlias :: TypeDefinition
columnAlias = define "ColumnAlias" $
  T.record [
    "column">: kql "ColumnName",
    "alias">: kql "ColumnName"]

columnAssignment :: TypeDefinition
columnAssignment = define "ColumnAssignment" $
  T.record [
    "column">: kql "ColumnName",
    "expression">: kql "Expression"]

columnName :: TypeDefinition
columnName = define "ColumnName" $ T.wrap T.string

columns :: TypeDefinition
columns = define "Columns" $
  T.union [
    "all">: T.unit,
    "single">: kql "ColumnName"]

command :: TypeDefinition
command = define "Command" $
  T.union [
    "count">: T.unit,
    "distinct">:
      doc "See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator" $
      nonemptyList $ kql "ColumnName",
    "extend">: nonemptyList $ kql "ColumnAssignment",
    "join">: kql "JoinCommand",
    "limit">: T.int32,
    "mvexpand">: kql "ColumnName",
    "orderBy">: nonemptyList $ kql "SortBy",
    "parse">: kql "ParseCommand",
    "print">: kql "PrintCommand",
    "project">: nonemptyList $ kql "Projection",
    "projectAway">: nonemptyList $ kql "ColumnName",
    "projectRename">: nonemptyList $ kql "ColumnAlias",
    "render">: T.string,
    "search">: kql "SearchCommand",
    "sortBy">: nonemptyList $ kql "SortBy",
    "summarize">: kql "SummarizeCommand",
    "take">:
      doc "Limit a search to a specified number of results"
      T.int32,
    "top">: kql "TopCommand",
    "union">: kql "UnionCommand",
    "where">: kql "Expression"]

datetime :: TypeDefinition
datetime = define "Datetime" $ T.wrap T.string

duration :: TypeDefinition
duration = define "Duration" $
  T.record [
    "value">: T.int32,
    "unit">: kql "DurationUnit"]

durationUnit :: TypeDefinition
durationUnit = define "DurationUnit" $
  T.enum ["second", "minute", "hour"]

expression :: TypeDefinition
expression = define "Expression" $
  T.union [
    "and">: nonemptyList $ kql "Expression",
    "any">: T.unit,
    "between">: kql "BetweenExpression",
    "binary">: kql "BinaryExpression",
    "braces">: kql "Expression", -- TODO: what do braces represent? E.g. "let timeRange = {TimeRange}"
    "column">: kql "ColumnName",
    "dataset">: kql "TableName",
    "index">: kql "IndexExpression",
    "list">: T.list $ kql "Expression",
    "literal">: kql "Literal",
    "or">: nonemptyList $ kql "Expression",
    "parentheses">: kql "Expression",
    "property">: kql "PropertyExpression",
    "unary">: kql "UnaryExpression"]

functionExpression :: TypeDefinition
functionExpression = define "FunctionExpression" $
  T.record [
    "function">: kql "Function",
    "arguments">: T.list $ kql "Expression"]

functionName :: TypeDefinition
functionName = define "FunctionName" $ T.wrap T.string

function_ :: TypeDefinition
function_ = define "Function" $
  T.union [
    "builtIn">: kql "BuiltInFunction",
    "custom">: kql "FunctionName"]

indexExpression :: TypeDefinition
indexExpression = define "IndexExpression" $
  T.record [
    "expression">: kql "Expression",
    "index">: T.string]

joinCommand :: TypeDefinition
joinCommand = define "JoinCommand" $
  T.record [
    "kind">: kql "JoinKind",
    "expression">: kql "TableName",
    "on">: kql "Expression"]

joinKind :: TypeDefinition
joinKind = define "JoinKind" $
  T.enum ["leftouter", "leftsemi", "leftanti", "fullouter", "inner", "innerunique", "rightouter", "rightsemi", "rightanti"]

keyValuePair :: TypeDefinition
keyValuePair = define "KeyValuePair" $
  T.record [
    "key">: T.string,
    "value">: kql "Expression"]

kql :: String -> Type
kql = typeref ns

letBinding :: TypeDefinition
letBinding = define "LetBinding" $
  T.record [
    "name">: kql "ColumnName",
    "expression">: kql "Expression"]

letExpression :: TypeDefinition
letExpression = define "LetExpression" $
  T.record [
    "bindings">: nonemptyList $ kql "LetBinding",
    "expression">: kql "TabularExpression"]

literal_ :: TypeDefinition
literal_ = define "Literal" $
  T.union [
    "duration">: kql "Duration",
    "datetime">: kql "Datetime",
    "string">: T.string,
    -- TODO: unverified
    "int">: T.int32,
    "long">: T.int64,
    "double">: T.float64,
    "boolean">: T.boolean]

order :: TypeDefinition
order = define "Order" $
  T.enum ["ascending", "descending"]

parameter :: TypeDefinition
parameter = define "Parameter" $
  T.record [
    "key">: T.string,
    "value">: kql "Literal"]

parseCommand :: TypeDefinition
parseCommand = define "ParseCommand" $
  T.record [
    "column">: kql "ColumnName",
    "pairs">: nonemptyList $ kql "KeyValuePair"]

-- TODO: what are these expressions actually called in KQL?
pipelineExpression :: TypeDefinition
pipelineExpression = define "PipelineExpression" $
  T.wrap $ nonemptyList $ kql "TabularExpression"

printCommand :: TypeDefinition
printCommand = define "PrintCommand" $
  T.record [
    "column">: T.maybe $ kql "ColumnName",
    "expression">: kql "Expression"]

projection :: TypeDefinition
projection = define "Projection" $
  T.record [
    "expression">: kql "Expression",
    "alias">: T.maybe $ kql "ColumnName"]

propertyExpression :: TypeDefinition
propertyExpression = define "PropertyExpression" $
  T.record [
    "expression">: kql "Expression",
    "property">: T.string]

query :: TypeDefinition
query = define "Query" $ T.wrap $ kql "TabularExpression"

searchCommand :: TypeDefinition
searchCommand = define "SearchCommand" $
  doc "Search across all datasets and columns or, if provided, specific datasets and/or columns" $
  T.record [
    "datasets">: T.list $ kql "TableName",
    "pattern">: kql "Expression"]

sortBy :: TypeDefinition
sortBy = define "SortBy" $
  T.record [
    "column">: kql "ColumnName",
    "order">: T.maybe $ kql "Order"]

summarizeCommand :: TypeDefinition
summarizeCommand = define "SummarizeCommand" $
  T.record [
     "columns">: nonemptyList $ kql "ColumnAssignment",
     "by">: T.list $ kql "ColumnName"]

tableName :: TypeDefinition
tableName = define "TableName" $ T.wrap T.string

tabularExpression :: TypeDefinition
tabularExpression = define "TabularExpression" $
  T.union [
    "command">: kql "Command",
    "pipeline">: kql "PipelineExpression",
    "let">: kql "LetExpression",
    "table">: kql "TableName"]

topCommand :: TypeDefinition
topCommand = define "TopCommand" $
  T.record [
    "count">: T.int32,
    "sort">: T.list $ kql "SortBy"]

unaryExpression :: TypeDefinition
unaryExpression = define "UnaryExpression" $
  T.record [
    "operator">: kql "UnaryOperator",
    "expression">: kql "Expression"]

unaryOperator :: TypeDefinition
unaryOperator = define "UnaryOperator" $
  T.enum ["not"]

unionCommand :: TypeDefinition
unionCommand = define "UnionCommand" $
  T.record [
    "parameters">: T.list $ kql "Parameter",
    "kind">: T.maybe $ kql "UnionKind",
    "withSource">: T.maybe $ kql "ColumnName",
    "isFuzzy">: T.maybe T.boolean,
    "tables">: nonemptyList $ kql "TableName"]

unionKind :: TypeDefinition
unionKind = define "UnionKind" $
  T.enum ["inner", "outer"]
