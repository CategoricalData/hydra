module Hydra.Sources.Kernel.Types.Imperative where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (assignmentTarget, declaration, expression, parameter, procedure, statement)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Module as Mod


ns :: Namespace
ns = Namespace "hydra.imperative"

define :: String -> Type -> Binding
define = defineType ns

imp :: String -> Type
imp = typeref ns

module_ :: Module
module_ = Module ns elements [Mod.ns] [Core.ns, Mod.ns]
    $ Just ("An imperative syntax model for use as an intermediate representation between Hydra's"
      ++ " functional core (hydra.core) and imperative target languages like Java, Python, and Scala.")
  where
    elements = [
      -- Module-level types
      imperativeModule,
      memberDefinition,
      procedureDefinition,

      -- Statement types
      assignment,
      assignmentTarget,
      classicFor,
      declaration,
      doExpression,
      expression,
      fieldTarget,
      forEach,
      forStatement,
      ifStatement,
      indexTarget,
      parameter,
      procedure,
      statement,
      whileStatement]

-- Module-level types ---------------------------------------------------------

imperativeModule :: Binding
imperativeModule = define "ImperativeModule" $
  doc ("An imperative module, containing type definitions from hydra.module"
    ++ " together with imperative procedure definitions") $
  T.record [
    "namespace">:
      doc "The namespace of the module"
      Mod.namespace,
    "typeDefinitions">:
      doc "Type definitions (records, unions, wraps) from the declarative model" $
      T.list Mod.typeDefinition,
    "memberDefinitions">:
      doc "Member definitions: procedures and constant values" $
      T.list memberDefinition,
    "dependencies">:
      doc "Namespaces of modules that this module depends on" $
      T.list Mod.namespace,
    "description">:
      doc "An optional human-readable description of the module" $
      T.maybe T.string]

memberDefinition :: Binding
memberDefinition = define "MemberDefinition" $
  doc "A member of an imperative module: either a procedure or a constant value" $
  T.union [
    "procedure">:
      doc "A procedure (imperative function) definition"
      procedureDefinition,
    "constant">:
      doc "A constant value definition (a term with its type)"
      Mod.termDefinition]

procedureDefinition :: Binding
procedureDefinition = define "ProcedureDefinition" $
  doc "A named procedure together with its type signature" $
  T.record [
    "name">:
      doc "The name of the procedure"
      Core.name,
    "procedure">:
      doc "The procedure itself"
      procedure,
    "typeScheme">:
      doc "The type scheme of the procedure"
      Core.typeScheme]

-- Expression types -----------------------------------------------------------

expression :: Binding
expression = define "Expression" $
  doc "An expression which may be either a pure declarative term or an imperative block that yields a value" $
  T.union [
    "pure">:
      doc "A pure declarative expression from the core language"
      Core.term,
    "do">:
      doc "An imperative block that executes statements and yields a value"
      doExpression]

doExpression :: Binding
doExpression = define "DoExpression" $
  doc "An imperative block that executes a sequence of statements and yields a final value" $
  T.record [
    "statements">:
      doc "The statements to execute in sequence" $
      T.list statement,
    "result">:
      doc "The final expression whose value is the result of the block"
      expression]

-- Statement types ------------------------------------------------------------

statement :: Binding
statement = define "Statement" $
  doc "A statement to be executed for its effects" $
  T.union [
    "assign">:
      doc "Assign a value to a target (variable, field, or index)"
      assignment,
    "block">:
      doc "A sequence of statements executed in order" $
      T.list statement,
    "break">:
      doc "Break out of the enclosing loop" $
      T.unit,
    "continue">:
      doc "Skip to the next iteration of the enclosing loop" $
      T.unit,
    "declare">:
      doc "Declare a new variable, optionally with a type and/or initial value"
      declaration,
    "expression">:
      doc "An expression evaluated for its side effects"
      expression,
    "for">:
      doc "A for loop (classic or for-each)"
      forStatement,
    "if">:
      doc "A conditional statement with optional else-if and else branches"
      ifStatement,
    "return">:
      doc "Return from the enclosing procedure, optionally with a value" $
      T.maybe expression,
    "while">:
      doc "A while loop"
      whileStatement]

assignment :: Binding
assignment = define "Assignment" $
  doc "An assignment of a value to a target" $
  T.record [
    "target">:
      doc "The location being assigned to"
      assignmentTarget,
    "value">:
      doc "The value to assign"
      expression]

assignmentTarget :: Binding
assignmentTarget = define "AssignmentTarget" $
  doc "A location which can be assigned to" $
  T.union [
    "variable">:
      doc "A simple variable"
      Core.name,
    "field">:
      doc "A field of an object"
      fieldTarget,
    "index">:
      doc "An element of an indexed collection"
      indexTarget]

fieldTarget :: Binding
fieldTarget = define "FieldTarget" $
  doc "A field of an object, used as an assignment target" $
  T.record [
    "object">:
      doc "The object whose field is being accessed"
      expression,
    "field">:
      doc "The name of the field"
      Core.name]

indexTarget :: Binding
indexTarget = define "IndexTarget" $
  doc "An element of an indexed collection, used as an assignment target" $
  T.record [
    "collection">:
      doc "The collection being indexed"
      expression,
    "index">:
      doc "The index expression"
      expression]

declaration :: Binding
declaration = define "Declaration" $
  doc "A variable declaration with optional type annotation and initial value" $
  T.record [
    "name">:
      doc "The name of the variable being declared"
      Core.name,
    "type">:
      doc "An optional type annotation" $
      T.maybe Core.type_,
    "value">:
      doc "An optional initial value" $
      T.maybe expression,
    "mutable">:
      doc "Whether the variable is mutable (true) or immutable (false)" $
      T.boolean]

-- Control flow types ---------------------------------------------------------

forStatement :: Binding
forStatement = define "ForStatement" $
  doc "A for loop, either classic C-style or for-each" $
  T.union [
    "classic">:
      doc "A classic C-style for loop: for(init; cond; update) body"
      classicFor,
    "forEach">:
      doc "A for-each loop: for(x in collection) body"
      forEach]

classicFor :: Binding
classicFor = define "ClassicFor" $
  doc "A classic C-style for loop with initialization, condition, update, and body" $
  T.record [
    "initialize">:
      doc "An optional initialization statement" $
      T.maybe statement,
    "condition">:
      doc "An optional loop condition" $
      T.maybe expression,
    "update">:
      doc "An optional update statement" $
      T.maybe statement,
    "body">:
      doc "The loop body"
      statement]

forEach :: Binding
forEach = define "ForEach" $
  doc "A for-each loop that iterates over a collection" $
  T.record [
    "variable">:
      doc "The loop variable"
      Core.name,
    "collection">:
      doc "The collection to iterate over"
      expression,
    "body">:
      doc "The loop body"
      statement]

ifStatement :: Binding
ifStatement = define "IfStatement" $
  doc "A conditional statement with an optional else-if chain and else branch" $
  T.record [
    "condition">:
      doc "The condition to test"
      expression,
    "body">:
      doc "The statement to execute if the condition is true"
      statement,
    "elseIfs">:
      doc "Zero or more else-if branches" $
      T.list ifStatement,
    "else">:
      doc "An optional else branch" $
      T.maybe statement]

whileStatement :: Binding
whileStatement = define "WhileStatement" $
  doc "A while loop that repeats as long as a condition holds" $
  T.record [
    "condition">:
      doc "The loop condition"
      expression,
    "body">:
      doc "The loop body"
      statement]

-- Procedure types ------------------------------------------------------------

procedure :: Binding
procedure = define "Procedure" $
  doc "An imperative function with parameters, an optional return type, and a statement body" $
  T.record [
    "name">:
      doc "The name of the procedure"
      Core.name,
    "parameters">:
      doc "The parameters of the procedure" $
      T.list parameter,
    "returnType">:
      doc "An optional return type" $
      T.maybe Core.type_,
    "body">:
      doc "The body of the procedure"
      statement]

parameter :: Binding
parameter = define "Parameter" $
  doc "A parameter of a procedure" $
  T.record [
    "name">:
      doc "The name of the parameter"
      Core.name,
    "type">:
      doc "The type of the parameter"
      Core.type_]
