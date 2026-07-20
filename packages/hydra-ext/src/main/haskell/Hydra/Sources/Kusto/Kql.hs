module Hydra.Sources.Kusto.Kql where

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
ns = ModuleName "hydra.kusto.kql"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative."))}
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
      sortBy,
      summarizeCommand,
      tableName,
      tabularExpression,
      topCommand,
      unaryExpression,
      unaryOperator,
      unionCommand,
      unionKind]

betweenExpression :: TypeDefinition
betweenExpression = define "BetweenExpression" $
  doc "A 'between' range test on an expression, optionally negated" $
  T.record [
    "not">: T.boolean,
    "expression">: kql "Expression",
    "lowerBound">: kql "Expression",
    "upperBound">: kql "Expression"]

binaryExpression :: TypeDefinition
binaryExpression = define "BinaryExpression" $
  doc "A binary operator expression, combining a left and right operand" $
  T.record [
    "left">: kql "Expression",
    "operator">: kql "BinaryOperator",
    "right">: kql "Expression"]

binaryOperator :: TypeDefinition
binaryOperator = define "BinaryOperator" $
  doc "A binary comparison or arithmetic operator used in KQL expressions" $
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
  doc "One of the built-in scalar or aggregation functions supported by KQL" $
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
  doc "An association between a column and an alternate (renamed) column name" $
  T.record [
    "column">: kql "ColumnName",
    "alias">: kql "ColumnName"]

columnAssignment :: TypeDefinition
columnAssignment = define "ColumnAssignment" $
  doc "An assignment of a computed expression to a column" $
  T.record [
    "column">: kql "ColumnName",
    "expression">: kql "Expression"]

columnName :: TypeDefinition
columnName = define "ColumnName" $
  doc "The name of a table column" $
  T.wrap T.string

columns :: TypeDefinition
columns = define "Columns" $
  doc "A selection of either all columns, or a single named column" $
  T.union [
    "all">: T.unit,
    "single">: kql "ColumnName"]

command :: TypeDefinition
command = define "Command" $
  doc "A tabular operator, applied in sequence within a pipeline expression" $
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
datetime = define "Datetime" $
  doc "A KQL datetime literal" $
  T.wrap T.string

duration :: TypeDefinition
duration = define "Duration" $
  doc "A duration (timespan) literal, expressed as a value and a unit" $
  T.record [
    "value">: T.int32,
    "unit">: kql "DurationUnit"]

durationUnit :: TypeDefinition
durationUnit = define "DurationUnit" $
  doc "The unit of measure of a Duration value" $
  T.enum ["second", "minute", "hour"]

expression :: TypeDefinition
expression = define "Expression" $
  doc "A KQL scalar expression" $
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
  doc "An invocation of a function with a list of argument expressions" $
  T.record [
    "function">: kql "Function",
    "arguments">: T.list $ kql "Expression"]

functionName :: TypeDefinition
functionName = define "FunctionName" $
  doc "The name of a user-defined function" $
  T.wrap T.string

function_ :: TypeDefinition
function_ = define "Function" $
  doc "A reference to either a built-in or a user-defined function" $
  T.union [
    "builtIn">: kql "BuiltInFunction",
    "custom">: kql "FunctionName"]

indexExpression :: TypeDefinition
indexExpression = define "IndexExpression" $
  doc "An indexing operation applied to an expression, e.g. array or property access" $
  T.record [
    "expression">: kql "Expression",
    "index">: T.string]

joinCommand :: TypeDefinition
joinCommand = define "JoinCommand" $
  doc "A 'join' operator combining rows from the current and a second tabular expression" $
  T.record [
    "kind">: kql "JoinKind",
    "expression">: kql "TableName",
    "on">: kql "Expression"]

joinKind :: TypeDefinition
joinKind = define "JoinKind" $
  doc "The kind of join to perform, e.g. inner, left outer, or left semi/anti" $
  T.enum ["leftouter", "leftsemi", "leftanti", "fullouter", "inner", "innerunique", "rightouter", "rightsemi", "rightanti"]

keyValuePair :: TypeDefinition
keyValuePair = define "KeyValuePair" $
  doc "A key-value pair extracted by a 'parse' command" $
  T.record [
    "key">: T.string,
    "value">: kql "Expression"]

kql :: String -> Type
kql = typeref ns

letBinding :: TypeDefinition
letBinding = define "LetBinding" $
  doc "A single 'let' binding of a name to an expression" $
  T.record [
    "name">: kql "ColumnName",
    "expression">: kql "Expression"]

letExpression :: TypeDefinition
letExpression = define "LetExpression" $
  doc "A sequence of 'let' bindings followed by a tabular expression that may reference them" $
  T.record [
    "bindings">: nonemptyList $ kql "LetBinding",
    "expression">: kql "TabularExpression"]

literal_ :: TypeDefinition
literal_ = define "Literal" $
  doc "A literal value in a KQL expression" $
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
  doc "The sort direction for an 'order by' or 'sort by' clause" $
  T.enum ["ascending", "descending"]

parameter :: TypeDefinition
parameter = define "Parameter" $
  doc "A named parameter with a literal value, e.g. for a union operator" $
  T.record [
    "key">: T.string,
    "value">: kql "Literal"]

parseCommand :: TypeDefinition
parseCommand = define "ParseCommand" $
  doc "A 'parse' command that extracts key-value pairs from a column into new columns" $
  T.record [
    "column">: kql "ColumnName",
    "pairs">: nonemptyList $ kql "KeyValuePair"]

-- TODO: what are these expressions actually called in KQL?
pipelineExpression :: TypeDefinition
pipelineExpression = define "PipelineExpression" $
  doc "A pipe ('|')-separated sequence of tabular expressions" $
  T.wrap $ nonemptyList $ kql "TabularExpression"

printCommand :: TypeDefinition
printCommand = define "PrintCommand" $
  doc "A 'print' command that outputs the value of an expression, optionally naming a column" $
  T.record [
    "column">: T.optional $ kql "ColumnName",
    "expression">: kql "Expression"]

projection :: TypeDefinition
projection = define "Projection" $
  doc "An expression to project as a column, with an optional alias, used by a 'project' command" $
  T.record [
    "expression">: kql "Expression",
    "alias">: T.optional $ kql "ColumnName"]

propertyExpression :: TypeDefinition
propertyExpression = define "PropertyExpression" $
  doc "A dotted property access on an expression, e.g. 'x.property'" $
  T.record [
    "expression">: kql "Expression",
    "property">: T.string]

query :: TypeDefinition
query = define "Query" $
  doc "A complete KQL query, consisting of a single tabular expression" $
  T.wrap $ kql "TabularExpression"

searchCommand :: TypeDefinition
searchCommand = define "SearchCommand" $
  doc "Search across all datasets and columns or, if provided, specific datasets and/or columns" $
  T.record [
    "datasets">: T.list $ kql "TableName",
    "pattern">: kql "Expression"]

sortBy :: TypeDefinition
sortBy = define "SortBy" $
  doc "A single sort key with an optional sort order, used by 'order by'/'sort by'" $
  T.record [
    "column">: kql "ColumnName",
    "order">: T.optional $ kql "Order"]

summarizeCommand :: TypeDefinition
summarizeCommand = define "SummarizeCommand" $
  doc "A 'summarize' command that computes aggregate columns, optionally grouped by other columns" $
  T.record [
     "columns">: nonemptyList $ kql "ColumnAssignment",
     "by">: T.list $ kql "ColumnName"]

tableName :: TypeDefinition
tableName = define "TableName" $
  doc "The name of a table or dataset" $
  T.wrap T.string

tabularExpression :: TypeDefinition
tabularExpression = define "TabularExpression" $
  doc "An expression which produces a table: a command, pipeline, let expression, or table reference" $
  T.union [
    "command">: kql "Command",
    "pipeline">: kql "PipelineExpression",
    "let">: kql "LetExpression",
    "table">: kql "TableName"]

topCommand :: TypeDefinition
topCommand = define "TopCommand" $
  doc "A 'top' command that returns the first N rows sorted by the given columns" $
  T.record [
    "count">: T.int32,
    "sort">: T.list $ kql "SortBy"]

unaryExpression :: TypeDefinition
unaryExpression = define "UnaryExpression" $
  doc "A unary operator expression applied to a single operand" $
  T.record [
    "operator">: kql "UnaryOperator",
    "expression">: kql "Expression"]

unaryOperator :: TypeDefinition
unaryOperator = define "UnaryOperator" $
  doc "A unary operator used in KQL expressions" $
  T.enum ["not"]

unionCommand :: TypeDefinition
unionCommand = define "UnionCommand" $
  doc "A 'union' command that combines rows from multiple tables" $
  T.record [
    "parameters">: T.list $ kql "Parameter",
    "kind">: T.optional $ kql "UnionKind",
    "withSource">: T.optional $ kql "ColumnName",
    "isFuzzy">: T.optional T.boolean,
    "tables">: nonemptyList $ kql "TableName"]

unionKind :: TypeDefinition
unionKind = define "UnionKind" $
  doc "Whether a union preserves only common columns (inner) or all columns (outer)" $
  T.enum ["inner", "outer"]
