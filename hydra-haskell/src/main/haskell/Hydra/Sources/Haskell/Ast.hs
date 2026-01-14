-- | A Haskell syntax model, loosely based on Language.Haskell.Tools.AST

module Hydra.Sources.Haskell.Ast where

-- Standard type-level imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.haskell.ast"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just "A Haskell syntax model, loosely based on Language.Haskell.Tools.AST"
  where
    elements = [
      alternative,
      assertion,
      classAssertion,
      caseRhs,
      constructor,
      ordinaryConstructor,
      recordConstructor,
      constructorWithComments,
      dataDeclaration,
      dataOrNewtype,
      declarationWithComments,
      declaration,
      declarationHead,
      applicationDeclarationHead,
      deriving_,
      export,
      expression,
      applicationExpression,
      caseExpression,
      constructRecordExpression,
      ifExpression,
      infixApplicationExpression,
      lambdaExpression,
      letExpression,
      prefixApplicationExpression,
      sectionExpression,
      typeSignatureExpression,
      updateRecordExpression,
      field,
      fieldWithComments,
      fieldUpdate,
      import_,
      specImport,
      importModifier,
      importExportSpec,
      subspecImportExportSpec,
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
      contextType,
      functionType,
      infixType,
      typeDeclaration,
      typeSignature,
      typedBinding,
      valueBinding,
      simpleValueBinding,
      variable]

alternative :: Binding -- UAlt
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

assertion :: Binding -- UAssertion (UClassAssert)
assertion = define "Assertion" $
  doc "A type assertion" $
  T.union [
    "class">:
      doc "A class assertion"
      classAssertion,
    "tuple">:
      doc "A tuple of assertions" $
      T.list assertion]
  -- omitted for now: implicit and infix assertions

classAssertion :: Binding -- UClassAssert
classAssertion = define "ClassAssertion" $
  doc "A class assertion" $
  T.record [
    "name">:
      doc "The name of the class"
      name,
    "types">:
      doc "The types to which the class is applied" $
      T.list type_]

caseRhs :: Binding -- UCaseRhs'
caseRhs = define "CaseRhs" $
  doc "The right-hand side of a pattern-matching alternative" $
  -- omitted for now: guarded
  T.wrap expression

constructor :: Binding -- UConDecl
constructor = define "Constructor" $
  doc "A data constructor" $
  -- omitted for now: ordinary (positional), infix
  T.union [
    "ordinary">:
      doc "An ordinary (positional) constructor"
      ordinaryConstructor,
    "record">:
      doc "A record constructor"
      recordConstructor]

ordinaryConstructor :: Binding
ordinaryConstructor = define "OrdinaryConstructor" $
  doc "An ordinary (positional) data constructor" $
  T.record [
    "name">:
      doc "The name of the constructor"
      name,
    "fields">:
      doc "The types of the positional fields" $
      T.list type_]

recordConstructor :: Binding
recordConstructor = define "RecordConstructor" $
  doc "A record-style data constructor" $
  T.record [
    "name">:
      doc "The name of the constructor"
      name,
    "fields">:
      doc "The named fields of the record" $
      T.list fieldWithComments]

constructorWithComments :: Binding
constructorWithComments = define "ConstructorWithComments" $
  doc "A data constructor together with any comments" $
  T.record [
    "body">:
      doc "The constructor"
      constructor,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

dataDeclaration :: Binding -- UDataDecl
dataDeclaration = define "DataDeclaration" $
  doc "A data type declaration" $
  T.record [
    "keyword">:
      doc "The 'data' or 'newtype' keyword"
      dataOrNewtype,
    "context">:
      doc "Type class constraints" $
      T.list assertion,
    "head">:
      doc "The declaration head"
      declarationHead,
    "constructors">:
      doc "The data constructors" $
      T.list constructorWithComments,
    "deriving">:
      doc "Derived type class instances" $
      T.list deriving_]

dataOrNewtype :: Binding
dataOrNewtype = define "DataOrNewtype" $
  doc "The 'data' versus 'newtype keyword" $
  T.enum ["data", "newtype"]

declarationWithComments :: Binding
declarationWithComments = define "DeclarationWithComments" $
  doc "A data declaration together with any comments" $
  T.record [
    "body">:
      doc "The declaration"
      declaration,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

declaration :: Binding -- UDecl
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
      typeDeclaration,
    "valueBinding">:
      doc "A value binding"
      valueBinding,
    "typedBinding">:
      doc "A typed binding"
      typedBinding]

declarationHead :: Binding -- UDeclHead
declarationHead = define "DeclarationHead" $
  doc "The left-hand side of a declaration" $
  -- omitted for now: infix application
  T.union [
    "application">:
      doc "An application-style declaration head"
      applicationDeclarationHead,
    "parens">:
      doc "A parenthesized declaration head"
      declarationHead,
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

deriving_ :: Binding -- UDeriving
deriving_ = define "Deriving" $
  doc "A 'deriving' statement" $
  -- omitted for now: infix, parenthesized, and application instance heads
  T.wrap $ T.list name

export :: Binding -- UExportSpec
export = define "Export" $
  doc "An export statement" $
  T.union [
    "declaration">:
      doc "An exported declaration"
      importExportSpec,
    "module">:
      doc "An exported module"
      moduleName]

expression :: Binding -- UExpr
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
      constructRecordExpression,
    "do">:
      doc "A 'do' expression" $
      T.list statement, -- omitted for now: do vs. mdo
    "if">:
      doc "An 'if' expression"
      ifExpression,
    "infixApplication">:
      doc "An infix application"
      infixApplicationExpression,
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
    "parens">:
      doc "A parenthesized expression"
      expression,
    "prefixApplication">:
      doc "A prefix application"
      prefixApplicationExpression,
    "rightSection">:
      doc "A right section expression"
      sectionExpression,
    "tuple">:
      doc "A tuple expression" $
      T.list expression,
    "typeSignature">:
      doc "A type signature expression"
      typeSignatureExpression,
    "updateRecord">:
      doc "A record update expression"
      updateRecordExpression,
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

constructRecordExpression :: Binding
constructRecordExpression = define "ConstructRecordExpression" $
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

infixApplicationExpression :: Binding
infixApplicationExpression = define "InfixApplicationExpression" $
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

prefixApplicationExpression :: Binding
prefixApplicationExpression = define "PrefixApplicationExpression" $
  doc "A prefix expression" $
  T.record [
    "operator">:
      doc "The prefix operator"
      operator,
    "rhs">:
      doc "The operand"
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

typeSignatureExpression :: Binding
typeSignatureExpression = define "TypeSignatureExpression" $
  doc "A type signature expression" $
  T.record [
    "inner">:
      doc "The expression being typed"
      expression,
    "type">:
      doc "The type signature"
      type_]

updateRecordExpression :: Binding
updateRecordExpression = define "UpdateRecordExpression" $
  doc "An update record expression" $
  T.record [
    "inner">:
      doc "The record being updated"
      expression,
    "fields">:
      doc "The field updates" $
      T.list fieldUpdate]

field :: Binding -- UFieldDecl
field = define "Field" $
  doc "A field (name/type pair)" $
  T.record [
    "name">:
      doc "The field name"
      name,
    "type">:
      doc "The field type"
      type_]

fieldWithComments :: Binding
fieldWithComments = define "FieldWithComments" $
  doc "A field together with any comments" $
  T.record [
    "field">:
      doc "The field"
      field,
    "comments">:
      doc "Optional comments" $
      T.maybe T.string]

fieldUpdate :: Binding -- UFieldUpdate
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

import_ :: Binding -- UImportDecl
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
      T.maybe specImport]

specImport :: Binding
specImport = define "SpecImport" $
  doc "An import specification" $
  T.union [
    "list">:
      doc "A list of imports to include" $
      T.list importExportSpec,
    "hiding">:
      doc "A list of imports to exclude" $
      T.list importExportSpec]

importModifier :: Binding -- UImportModifier
importModifier = define "ImportModifier" $
  doc "An import modifier ('pattern' or 'type')" $
  T.enum ["pattern", "type"]

importExportSpec :: Binding -- UIESpec
importExportSpec = define "ImportExportSpec" $
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
      T.maybe subspecImportExportSpec]

subspecImportExportSpec :: Binding
subspecImportExportSpec = define "SubspecImportExportSpec" $
  doc "A subspecification within an import/export" $
  T.union [
    "all">:
      doc "Import/export all"
      T.unit,
    "list">:
      doc "Import/export specific names" $
      T.list name]

literal :: Binding -- ULiteral
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

localBinding :: Binding -- ULocalBind
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

localBindings :: Binding -- ULocalBinds
localBindings = define "LocalBindings" $
  doc "A collection of local bindings" $
  T.wrap $ T.list localBinding

module' :: Binding -- UModule
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
      T.list declarationWithComments]

moduleHead :: Binding -- UModuleHead
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
      T.list export] -- UExportSpecs

moduleName :: Binding -- UModuleName
moduleName = define "ModuleName" $
  doc "A module name" $
  T.wrap T.string

name :: Binding -- UName
name = define "Name" $
  doc "A name" $
  T.union [
    "implicit">:
      doc "An implicit name"
      qualifiedName,
    "normal">:
      doc "A normal name"
      qualifiedName,
    "parens">:
      doc "A parenthesized name"
      qualifiedName]

namePart :: Binding -- UNamePart
namePart = define "NamePart" $
  doc "A component of a qualified name" $
  T.wrap T.string

operator :: Binding -- UOperator
operator = define "Operator" $
  doc "An operator" $
  T.union [
    "backtick">:
      doc "A function used as an infix operator"
      qualifiedName,
    "normal">:
      doc "A normal infix operator"
      qualifiedName]

pattern :: Binding -- UPattern
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
    "parens">:
      doc "A parenthesized pattern"
      pattern,
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

patternField :: Binding -- UPatternField
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

qualifiedName :: Binding -- UQualifiedName
qualifiedName = define "QualifiedName" $
  doc "A qualified name" $
  T.record [
    "qualifiers">:
      doc "The qualifier parts" $
      T.list namePart,
    "unqualified">:
      doc "The unqualified name part"
      namePart]

rightHandSide :: Binding -- URhs
rightHandSide = define "RightHandSide" $
  doc "A right-hand side of a binding" $
  -- omitted for now: guarded rhs
  T.wrap expression

statement :: Binding -- UStmt
statement = define "Statement" $
  doc "A do-notation statement" $
  T.wrap expression

type_ :: Binding -- UType
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
      contextType,
    "function">:
      doc "A function type"
      functionType,
    "infix">:
      doc "An infix type"
      infixType,
    "list">:
      doc "A list type"
      type_,
    "parens">:
      doc "A parenthesized type"
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

contextType :: Binding
contextType = define "ContextType" $
  doc "A type with a context (type class constraints)" $
  T.record [
    "ctx">:
      doc "The type class context"
      assertion, -- UContext
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
      doc "The right-hand operator"
      operator]

typeDeclaration :: Binding -- UTypeDecl
typeDeclaration = define "TypeDeclaration" $
  doc "A type synonym declaration" $
  T.record [
    "name">:
      doc "The declaration head"
      declarationHead,
    "type">:
      doc "The type being defined"
      type_]

typeSignature :: Binding -- UTypeSignature
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
      valueBinding]

valueBinding :: Binding -- UValueBind
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
      T.maybe localBindings]

variable :: Binding
variable = define "Variable" $
  doc "A type variable" $
  -- omitted for now: kind constraints
  T.wrap name
