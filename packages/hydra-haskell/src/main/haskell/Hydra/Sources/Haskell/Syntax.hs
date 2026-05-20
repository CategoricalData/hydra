-- | A Haskell syntax model for Hydra.
-- | Originally inspired by Language.Haskell.Tools.AST, but now diverges freely to suit Hydra's needs.

module Hydra.Sources.Haskell.Syntax where

-- Standard type-level imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.haskell.syntax"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = [Core.ns],
            moduleDescription = Just "A Haskell syntax model for Hydra. Originally inspired by Language.Haskell.Tools.AST, but now diverges freely to suit Hydra's needs."}
  where
    definitions = [
      alternative,
      constraint,
      classConstraint,
      caseRhs,
      constructor,
      positionalConstructor,
      recordConstructor,
      dataDeclaration,
      dataKeyword,
      declaration,
      declarationHead,
      applicationDeclarationHead,
      derivingClause,
      export,
      expression,
      applicationExpression,
      caseExpression,
      recordExpression,
      ifExpression,
      infixExpression,
      lambdaExpression,
      letExpression,
      sectionExpression,
      typedExpression,
      recordUpdateExpression,
      field,
      fieldUpdate,
      import_,
      importSpec,
      importModifier,
      namedImportExport,
      importExportSubspec,
      literal,
      localBinding,
      localBindings,
      module',
      moduleHead,
      moduleName,
      name,
      namePart,
      operator,
      pattern,
      applicationPattern,
      asPattern,
      recordPattern,
      typedPattern,
      patternField,
      qualifiedName,
      rightHandSide,
      statement,
      type_,
      applicationType,
      constrainedType,
      functionType,
      infixType,
      typeSynonymDeclaration,
      typeSignature,
      typedBinding,
      valueBinding,
      simpleValueBinding,
      variable]

alternative :: Binding
alternative = define "Alternative" $
  doc "A pattern-matching alternative" $
  T.record [
    "pattern">:
      doc "The pattern to match"
      pattern,
    "rhs">:
      doc "The right-hand side of the alternative"
      caseRhs,
    "binds">:
      doc "Optional local bindings" $
      T.maybe localBindings]

constraint :: Binding
constraint = define "Constraint" $
  doc "A type constraint" $
  T.union [
    "class">:
      doc "A class constraint"
      classConstraint,
    "tuple">:
      doc "A tuple of constraints" $
      T.list constraint]
  -- omitted for now: implicit and infix constraints

classConstraint :: Binding
classConstraint = define "ClassConstraint" $
  doc "A class constraint" $
  T.record [
    "name">:
      doc "The name of the class"
      name,
    "types">:
      doc "The types to which the class is applied" $
      T.list type_]

caseRhs :: Binding
caseRhs = define "CaseRhs" $
  doc "The right-hand side of a pattern-matching alternative" $
  -- omitted for now: guarded
  T.wrap expression

constructor :: Binding
constructor = define "Constructor" $
  doc "A data constructor" $
  -- omitted for now: ordinary (positional), infix
  T.union [
    "ordinary">:
      doc "An ordinary (positional) constructor"
      positionalConstructor,
    "record">:
      doc "A record constructor"
      recordConstructor]

positionalConstructor :: Binding
positionalConstructor = define "PositionalConstructor" $
  doc "An ordinary (positional) data constructor" $
  T.record [
    "name">:
      doc "The name of the constructor"
      name,
    "fields">:
      doc "The types of the positional fields" $
      T.list type_,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

recordConstructor :: Binding
recordConstructor = define "RecordConstructor" $
  doc "A record-style data constructor" $
  T.record [
    "name">:
      doc "The name of the constructor"
      name,
    "fields">:
      doc "The named fields of the record" $
      T.list field,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

dataDeclaration :: Binding
dataDeclaration = define "DataDeclaration" $
  doc "A data type declaration" $
  T.record [
    "keyword">:
      doc "The 'data' or 'newtype' keyword"
      dataKeyword,
    "context">:
      doc "Type class constraints" $
      T.list constraint,
    "head">:
      doc "The declaration head"
      declarationHead,
    "constructors">:
      doc "The data constructors" $
      T.list constructor,
    "deriving">:
      doc "Derived type class instances" $
      T.list derivingClause,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

dataKeyword :: Binding
dataKeyword = define "DataKeyword" $
  doc "The 'data' versus 'newtype keyword" $
  T.enum ["data", "newtype"]

declaration :: Binding
declaration = define "Declaration" $
  doc "A data or value declaration" $
  -- omitted for now: typeFamily, typeSignature, closedTypeFamily, gDataDecl, typeInst, dataInst, gDataInst, class, inst,
  --                  patternSynonym, deriv, fixity, default, patTypeSig, foreignImport, foreignExport, pragma,
  --                  role, splice
  T.union [
    "data">:
      doc "A data type declaration"
      dataDeclaration,
    "type">:
      doc "A type synonym declaration"
      typeSynonymDeclaration,
    "valueBinding">:
      doc "A value binding"
      valueBinding,
    "typedBinding">:
      doc "A typed binding"
      typedBinding]

declarationHead :: Binding
declarationHead = define "DeclarationHead" $
  doc "The left-hand side of a declaration" $
  -- omitted for now: infix application
  T.union [
    "application">:
      doc "An application-style declaration head"
      applicationDeclarationHead,
    "simple">:
      doc "A simple name"
      name]

applicationDeclarationHead :: Binding
applicationDeclarationHead = define "ApplicationDeclarationHead" $
  doc "An application-style declaration head" $
  T.record [
    "function">:
      doc "The function being applied"
      declarationHead,
    "operand">:
      doc "The type variable operand"
      variable]

derivingClause :: Binding
derivingClause = define "DerivingClause" $
  doc "A 'deriving' clause" $
  -- omitted for now: infix, parenthesized, and application instance heads
  T.wrap $ T.list name

export :: Binding
export = define "Export" $
  doc "An export statement" $
  T.union [
    "declaration">:
      doc "An exported declaration"
      namedImportExport,
    "module">:
      doc "An exported module"
      moduleName]

expression :: Binding
expression = define "Expression" $
  doc "A data expression" $
  -- omitted for now: multi-if, unboxed tuple, tuple section, unboxed tuple section, parallel array,
  --                  enum, parallel array enum, list comp, parallel array comp, type application,
  --                  (all Template Haskell constructors), pragma, arrow definition, arrow application,
  --                  lambda cases, static, unboxed sum, hole
  T.union [
    "application">:
      doc "A function application"
      applicationExpression,
    "case">:
      doc "A case expression"
      caseExpression,
    "constructRecord">:
      doc "A record constructor expression"
      recordExpression,
    "do">:
      doc "A 'do' expression" $
      T.list statement, -- omitted for now: do vs. mdo
    "if">:
      doc "An 'if' expression"
      ifExpression,
    "infixApplication">:
      doc "An infix application"
      infixExpression,
    "literal">:
      doc "A literal value"
      literal,
    "lambda">:
      doc "A lambda expression"
      lambdaExpression,
    "leftSection">:
      doc "A left section expression"
      sectionExpression,
    "let">:
      doc "A 'let' expression"
      letExpression,
    "list">:
      doc "A list expression" $
      T.list expression,
    "rightSection">:
      doc "A right section expression"
      sectionExpression,
    "tuple">:
      doc "A tuple expression" $
      T.list expression,
    "typeSignature">:
      doc "A type signature expression"
      typedExpression,
    "updateRecord">:
      doc "A record update expression"
      recordUpdateExpression,
    "variable">:
      doc "A variable reference"
      name]

applicationExpression :: Binding
applicationExpression = define "ApplicationExpression" $
  doc "An application expression" $
  T.record [
    "function">:
      doc "The function being applied"
      expression,
    "argument">:
      doc "The argument"
      expression]

caseExpression :: Binding
caseExpression = define "CaseExpression" $
  doc "A case expression" $
  T.record [
    "case">:
      doc "The expression being matched"
      expression,
    "alternatives">:
      doc "The pattern-matching alternatives" $
      T.list alternative]

recordExpression :: Binding
recordExpression = define "RecordExpression" $
  doc "A record constructor expression" $
  T.record [
    "name">:
      doc "The constructor name"
      name,
    "fields">:
      doc "The field assignments" $
      T.list fieldUpdate]

ifExpression :: Binding
ifExpression = define "IfExpression" $
  doc "An 'if' expression" $
  T.record [
    "condition">:
      doc "The condition expression"
      expression,
    "then">:
      doc "The 'then' branch"
      expression,
    "else">:
      doc "The 'else' branch"
      expression]

infixExpression :: Binding
infixExpression = define "InfixExpression" $
  doc "An infix application expression" $
  T.record [
    "lhs">:
      doc "The left-hand operand"
      expression,
    "operator">:
      doc "The infix operator"
      operator,
    "rhs">:
      doc "The right-hand operand"
      expression]

lambdaExpression :: Binding
lambdaExpression = define "LambdaExpression" $
  doc "A lambda expression" $
  T.record [
    "bindings">:
      doc "The patterns binding parameters" $
      T.list pattern,
    "inner">:
      doc "The body of the lambda"
      expression]

letExpression :: Binding
letExpression = define "LetExpression" $
  doc "A 'let' expression" $
  T.record [
    "bindings">:
      doc "The local bindings" $
      T.list localBinding,
    "inner">:
      doc "The body of the let expression"
      expression]

sectionExpression :: Binding
sectionExpression = define "SectionExpression" $
  doc "A section expression" $
  T.record [
    "operator">:
      doc "The operator"
      operator,
    "expression">:
      doc "The operand"
      expression]

typedExpression :: Binding
typedExpression = define "TypedExpression" $
  doc "A type signature expression" $
  T.record [
    "inner">:
      doc "The expression being typed"
      expression,
    "type">:
      doc "The type signature"
      type_]

recordUpdateExpression :: Binding
recordUpdateExpression = define "RecordUpdateExpression" $
  doc "An update record expression" $
  T.record [
    "inner">:
      doc "The record being updated"
      expression,
    "fields">:
      doc "The field updates" $
      T.list fieldUpdate]

field :: Binding
field = define "Field" $
  doc "A field (name/type pair)" $
  T.record [
    "name">:
      doc "The field name"
      name,
    "type">:
      doc "The field type"
      type_,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

fieldUpdate :: Binding
fieldUpdate = define "FieldUpdate" $
  doc "A field name and value" $
  -- omitted for now: pun, wildcard
  T.record [
    "name">:
      doc "The field name"
      name,
    "value">:
      doc "The field value"
      expression]

import_ :: Binding
import_ = define "Import" $
  doc "An import statement" $
  -- omitted for now: source, safe, pkg
  T.record [
    "qualified">:
      doc "Whether the import is qualified"
      T.boolean,
    "module">:
      doc "The module being imported"
      moduleName,
    "as">:
      doc "Optional alias for the module" $
      T.maybe moduleName,
    "spec">:
      doc "Optional import specification" $
      T.maybe importSpec]

importSpec :: Binding
importSpec = define "ImportSpec" $
  doc "An import specification" $
  T.union [
    "list">:
      doc "A list of imports to include" $
      T.list namedImportExport,
    "hiding">:
      doc "A list of imports to exclude" $
      T.list namedImportExport]

importModifier :: Binding
importModifier = define "ImportModifier" $
  doc "An import modifier ('pattern' or 'type')" $
  T.enum ["pattern", "type"]

namedImportExport :: Binding
namedImportExport = define "NamedImportExport" $
  doc "An import or export specification" $
  T.record [
    "modifier">:
      doc "Optional import modifier" $
      T.maybe importModifier,
    "name">:
      doc "The name being imported or exported"
      name,
    "subspec">:
      doc "Optional subspecification" $
      T.maybe importExportSubspec]

importExportSubspec :: Binding
importExportSubspec = define "ImportExportSubspec" $
  doc "A subspecification within an import/export" $
  T.union [
    "all">:
      doc "Import/export all"
      T.unit,
    "list">:
      doc "Import/export specific names" $
      T.list name]

literal :: Binding
literal = define "Literal" $
  doc "A literal value" $
  -- omitted for now: frac, primChar
  T.union [
    "char">:
      doc "A character literal"
      T.uint16,
    "double">:
      doc "A double-precision floating point literal"
      T.float64,
    "float">:
      doc "A single-precision floating point literal"
      T.float32,
    "int">:
      doc "A 32-bit integer literal"
      T.int32,
    "integer">:
      doc "An arbitrary-precision integer literal"
      T.bigint,
    "string">:
      doc "A string literal"
      T.string]

localBinding :: Binding
localBinding = define "LocalBinding" $
  doc "A local binding" $
  -- omitted for now: fixity, pragma
  T.union [
    "signature">:
      doc "A type signature"
      typeSignature,
    "value">:
      doc "A value binding"
      valueBinding]

localBindings :: Binding
localBindings = define "LocalBindings" $
  doc "A collection of local bindings" $
  T.wrap $ T.list localBinding

module' :: Binding
module' = define "Module" $
  doc "A Haskell module" $
  -- omitted for now: pragma
  T.record [
    "head">:
      doc "Optional module head" $
      T.maybe moduleHead,
    "imports">:
      doc "Import statements" $
      T.list import_,
    "declarations">:
      doc "Module declarations" $
      T.list declaration]

moduleHead :: Binding
moduleHead = define "ModuleHead" $
  doc "A module head" $
  -- omitted for now: pragma
  T.record [
    "comments">:
      doc "Optional module-level comments" $
      T.maybe T.string,
    "name">:
      doc "The module name"
      moduleName,
    "exports">:
      doc "Export list" $
      T.list export]

moduleName :: Binding
moduleName = define "ModuleName" $
  doc "A module name" $
  T.wrap T.string

name :: Binding
name = define "Name" $
  doc "A name" $
  T.union [
    "implicit">:
      doc "An implicit name"
      qualifiedName,
    "normal">:
      doc "A normal name"
      qualifiedName]

namePart :: Binding
namePart = define "NamePart" $
  doc "A component of a qualified name" $
  T.wrap T.string

operator :: Binding
operator = define "Operator" $
  doc "An operator" $
  T.union [
    "backtick">:
      doc "A function used as an infix operator"
      qualifiedName,
    "normal">:
      doc "A normal infix operator"
      qualifiedName]

pattern :: Binding
pattern = define "Pattern" $
  doc "A pattern" $
  -- omitted for now: unboxed tuples, parallel arrays, irrefutable, bang, view, splice, quasiquote, plusk, unboxed sum
  T.union [
    "application">:
      doc "An application pattern"
      applicationPattern,
    "as">:
      doc "An 'as' pattern"
      asPattern,
    "list">:
      doc "A list pattern" $
      T.list pattern,
    "literal">:
      doc "A literal pattern"
      literal,
    "name">:
      doc "A name pattern"
      name,
    "record">:
      doc "A record pattern"
      recordPattern,
    "tuple">:
      doc "A tuple pattern" $
      T.list pattern,
    "typed">:
      doc "A typed pattern"
      typedPattern,
    "wildcard">:
      doc "A wildcard pattern"
      T.unit]

applicationPattern :: Binding
applicationPattern = define "ApplicationPattern" $
  doc "An application pattern" $
  T.record [
    "name">:
      doc "The constructor name"
      name,
    "args">:
      doc "The pattern arguments" $
      T.list pattern]

asPattern :: Binding
asPattern = define "AsPattern" $
  doc "An 'as' pattern" $
  T.record [
    "name">:
      doc "The bound name"
      name,
    "inner">:
      doc "The inner pattern"
      pattern]

recordPattern :: Binding
recordPattern = define "RecordPattern" $
  doc "A record pattern" $
  T.record [
    "name">:
      doc "The constructor name"
      name,
    "fields">:
      doc "The field patterns" $
      T.list patternField]

typedPattern :: Binding
typedPattern = define "TypedPattern" $
  doc "A typed pattern" $
  T.record [
    "inner">:
      doc "The inner pattern"
      pattern,
    "type">:
      doc "The type annotation"
      type_]

patternField :: Binding
patternField = define "PatternField" $
  doc "A pattern field" $
  -- omitted for now: puns, wildcards
  T.record [
    "name">:
      doc "The field name"
      name,
    "pattern">:
      doc "The field pattern"
      pattern]

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  doc "A qualified name" $
  T.record [
    "qualifiers">:
      doc "The qualifier parts" $
      T.list namePart,
    "unqualified">:
      doc "The unqualified name part"
      namePart]

rightHandSide :: Binding
rightHandSide = define "RightHandSide" $
  doc "A right-hand side of a binding" $
  -- omitted for now: guarded rhs
  T.wrap expression

statement :: Binding
statement = define "Statement" $
  doc "A do-notation statement" $
  T.wrap expression

type_ :: Binding
type_ = define "Type" $
  doc "A type expression" $
  -- omitted for now: forall, unboxed tuple, parallel array, kinded, promoted, splice, quasiquote, bang,
  --                  lazy, unpack, nounpack, wildcard, named wildcard, sum
  T.union [
    "application">:
      doc "An application type"
      applicationType,
    "ctx">:
      doc "A context type"
      constrainedType,
    "function">:
      doc "A function type"
      functionType,
    "infix">:
      doc "An infix type"
      infixType,
    "list">:
      doc "A list type"
      type_,
    "tuple">:
      doc "A tuple type" $
      T.list type_,
    "variable">:
      doc "A type variable or type name"
      name]

applicationType :: Binding
applicationType = define "ApplicationType" $
  doc "An application type" $
  T.record [
    "context">:
      doc "The type being applied"
      type_,
    "argument">:
      doc "The type argument"
      type_]

constrainedType :: Binding
constrainedType = define "ConstrainedType" $
  doc "A type with a context (type class constraints)" $
  T.record [
    "ctx">:
      doc "The type class context"
      constraint,
    "type">:
      doc "The constrained type"
      type_]

functionType :: Binding
functionType = define "FunctionType" $
  doc "A function type" $
  T.record [
    "domain">:
      doc "The domain type"
      type_,
    "codomain">:
      doc "The codomain type"
      type_]

infixType :: Binding
infixType = define "InfixType" $
  doc "An infix type application" $
  T.record [
    "lhs">:
      doc "The left-hand type"
      type_,
    "operator">:
      doc "The type operator"
      operator,
    "rhs">:
      doc "The right-hand type"
      type_]

typeSynonymDeclaration :: Binding
typeSynonymDeclaration = define "TypeSynonymDeclaration" $
  doc "A type synonym declaration" $
  T.record [
    "name">:
      doc "The declaration head"
      declarationHead,
    "type">:
      doc "The type being defined"
      type_,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

typeSignature :: Binding
typeSignature = define "TypeSignature" $
  doc "A type signature" $
  T.record [
    "name">:
      doc "The name being typed"
      name,
    "type">:
      doc "The type"
      type_]

typedBinding :: Binding -- Added for convenience
typedBinding = define "TypedBinding" $
  doc "A binding with its type signature" $
  T.record [
    "typeSignature">:
      doc "The type signature"
      typeSignature,
    "valueBinding">:
      doc "The value binding"
      valueBinding,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

valueBinding :: Binding
valueBinding = define "ValueBinding" $
  doc "A value binding" $
  -- omitted for now: funBind
  T.union [
    "simple">:
      doc "A simple value binding"
      simpleValueBinding]

simpleValueBinding :: Binding
simpleValueBinding = define "SimpleValueBinding" $
  doc "A simple value binding" $
  T.record [
    "pattern">:
      doc "The pattern being bound"
      pattern,
    "rhs">:
      doc "The right-hand side"
      rightHandSide,
    "localBindings">:
      doc "Optional local bindings (where clause)" $
      T.maybe localBindings,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

variable :: Binding
variable = define "Variable" $
  doc "A type variable" $
  -- omitted for now: kind constraints
  T.wrap name
