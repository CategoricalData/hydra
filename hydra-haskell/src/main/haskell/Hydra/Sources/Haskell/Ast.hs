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
module_ = Module ns elements [Core.module_] [Core.module_] $
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
    "pattern">: pattern,
    "rhs">: caseRhs,
    "binds">: T.maybe localBindings]

assertion :: Binding -- UAssertion (UClassAssert)
assertion = define "Assertion" $
  doc "A type assertion" $
  T.union [
    "class">: classAssertion,
    "tuple">: T.list assertion]
  -- omitted for now: implicit and infix assertions

classAssertion :: Binding -- UClassAssert
classAssertion = define "ClassAssertion" $
  T.record [
    "name">: name,
    "types">: T.list type_]

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
    "ordinary">: ordinaryConstructor,
    "record">: recordConstructor]

ordinaryConstructor :: Binding
ordinaryConstructor = define "OrdinaryConstructor" $
  doc "An ordinary (positional) data constructor" $
  T.record [
    "name">: name,
    "fields">: T.list type_]

recordConstructor :: Binding
recordConstructor = define "RecordConstructor" $
  doc "A record-style data constructor" $
  T.record [
    "name">: name,
    "fields">: T.list fieldWithComments]

constructorWithComments :: Binding
constructorWithComments = define "ConstructorWithComments" $
  doc "A data constructor together with any comments" $
  T.record [
    "body">: constructor,
    "comments">: T.maybe T.string]

dataDeclaration :: Binding -- UDataDecl
dataDeclaration = define "DataDeclaration" $
  doc "A data type declaration" $
  T.record [
    "keyword">: dataOrNewtype,
    "context">: T.list assertion,
    "head">: declarationHead,
    "constructors">: T.list constructorWithComments,
    "deriving">: T.list deriving_]

dataOrNewtype :: Binding
dataOrNewtype = define "DataOrNewtype" $
  doc "The 'data' versus 'newtype keyword" $
  T.enum ["data", "newtype"]

declarationWithComments :: Binding
declarationWithComments = define "DeclarationWithComments" $
  doc "A data declaration together with any comments" $
  T.record [
    "body">: declaration,
    "comments">: T.maybe T.string]

declaration :: Binding -- UDecl
declaration = define "Declaration" $
  doc "A data or value declaration" $
  -- omitted for now: typeFamily, typeSignature, closedTypeFamily, gDataDecl, typeInst, dataInst, gDataInst, class, inst,
  --                  patternSynonym, deriv, fixity, default, patTypeSig, foreignImport, foreignExport, pragma,
  --                  role, splice
  T.union [
    "data">: dataDeclaration,
    "type">: typeDeclaration,
    "valueBinding">: valueBinding,
    "typedBinding">: typedBinding]

declarationHead :: Binding -- UDeclHead
declarationHead = define "DeclarationHead" $
  doc "The left-hand side of a declaration" $
  -- omitted for now: infix application
  T.union [
    "application">: applicationDeclarationHead,
    "parens">: declarationHead,
    "simple">: name]

applicationDeclarationHead :: Binding
applicationDeclarationHead = define "ApplicationDeclarationHead" $
  doc "An application-style declaration head" $
  T.record [
    "function">: declarationHead,
    "operand">: variable]

deriving_ :: Binding -- UDeriving
deriving_ = define "Deriving" $
  doc "A 'deriving' statement" $
  -- omitted for now: infix, parenthesized, and application instance heads
  T.wrap $ T.list name

export :: Binding -- UExportSpec
export = define "Export" $
  doc "An export statement" $
  T.union [
    "declaration">: importExportSpec,
    "module">: moduleName]

expression :: Binding -- UExpr
expression = define "Expression" $
  doc "A data expression" $
  -- omitted for now: multi-if, unboxed tuple, tuple section, unboxed tuple section, parallel array,
  --                  enum, parallel array enum, list comp, parallel array comp, type application,
  --                  (all Template Haskell constructors), pragma, arrow definition, arrow application,
  --                  lambda cases, static, unboxed sum, hole
  T.union [
    "application">: applicationExpression,
    "case">: caseExpression,
    "constructRecord">: constructRecordExpression,
    "do">: T.list statement, -- omitted for now: do vs. mdo
    "if">: ifExpression,
    "infixApplication">: infixApplicationExpression,
    "literal">: literal,
    "lambda">: lambdaExpression,
    "leftSection">: sectionExpression,
    "let">: letExpression,
    "list">: T.list expression,
    "parens">: expression,
    "prefixApplication">: prefixApplicationExpression,
    "rightSection">: sectionExpression,
    "tuple">: T.list expression,
    "typeSignature">: typeSignatureExpression,
    "updateRecord">: updateRecordExpression,
    "variable">: name]

applicationExpression :: Binding
applicationExpression = define "ApplicationExpression" $
  doc "An application expression" $
  T.record [
    "function">: expression,
    "argument">: expression]

caseExpression :: Binding
caseExpression = define "CaseExpression" $
  doc "A case expression" $
  T.record [
    "case">: expression,
    "alternatives">: T.list alternative]

constructRecordExpression :: Binding
constructRecordExpression = define "ConstructRecordExpression" $
  doc "A record constructor expression" $
  T.record [
    "name">: name,
    "fields">: T.list fieldUpdate]

ifExpression :: Binding
ifExpression = define "IfExpression" $
  doc "An 'if' expression" $
  T.record [
    "condition">: expression,
    "then">: expression,
    "else">: expression]

infixApplicationExpression :: Binding
infixApplicationExpression = define "InfixApplicationExpression" $
  doc "An infix application expression" $
  T.record [
    "lhs">: expression,
    "operator">: operator,
    "rhs">: expression]

lambdaExpression :: Binding
lambdaExpression = define "LambdaExpression" $
  doc "A lambda expression" $
  T.record [
    "bindings">: T.list pattern,
    "inner">: expression]

letExpression :: Binding
letExpression = define "LetExpression" $
  doc "A 'let' expression" $
  T.record [
    "bindings">: T.list localBinding,
    "inner">: expression]

prefixApplicationExpression :: Binding
prefixApplicationExpression = define "PrefixApplicationExpression" $
  doc "A prefix expression" $
  T.record [
    "operator">: operator,
    "rhs">: expression]

sectionExpression :: Binding
sectionExpression = define "SectionExpression" $
  doc "A section expression" $
  T.record [
    "operator">: operator,
    "expression">: expression]

typeSignatureExpression :: Binding
typeSignatureExpression = define "TypeSignatureExpression" $
  doc "A type signature expression" $
  T.record [
    "inner">: expression,
    "type">: type_]

updateRecordExpression :: Binding
updateRecordExpression = define "UpdateRecordExpression" $
  doc "An update record expression" $
  T.record [
    "inner">: expression,
    "fields">: T.list fieldUpdate]

field :: Binding -- UFieldDecl
field = define "Field" $
  doc "A field (name/type pair)" $
  T.record [
    "name">: name,
    "type">: type_]

fieldWithComments :: Binding
fieldWithComments = define "FieldWithComments" $
  doc "A field together with any comments" $
  T.record [
    "field">: field,
    "comments">: T.maybe T.string]

fieldUpdate :: Binding -- UFieldUpdate
fieldUpdate = define "FieldUpdate" $
  doc "A field name and value" $
  -- omitted for now: pun, wildcard
  T.record [
    "name">: name,
    "value">: expression]

import_ :: Binding -- UImportDecl
import_ = define "Import" $
  doc "An import statement" $
  -- omitted for now: source, safe, pkg
  T.record [
    "qualified">: T.boolean,
    "module">: moduleName,
    "as">: T.maybe moduleName,
    "spec">: T.maybe specImport]

specImport :: Binding
specImport = define "SpecImport" $
  doc "An import specification" $
  T.union [
    "list">: T.list importExportSpec,
    "hiding">: T.list importExportSpec]

importModifier :: Binding -- UImportModifier
importModifier = define "ImportModifier" $
  doc "An import modifier ('pattern' or 'type')" $
  T.enum ["pattern", "type"]

importExportSpec :: Binding -- UIESpec
importExportSpec = define "ImportExportSpec" $
  doc "An import or export specification" $
  T.record [
    "modifier">: T.maybe importModifier,
    "name">: name,
    "subspec">: T.maybe subspecImportExportSpec]

subspecImportExportSpec :: Binding
subspecImportExportSpec = define "SubspecImportExportSpec" $
  T.union [
    "all">: T.unit,
    "list">: T.list name]

literal :: Binding -- ULiteral
literal = define "Literal" $
  doc "A literal value" $
  -- omitted for now: frac, primChar
  T.union [
    "char">: T.uint16,
    "double">: T.float64,
    "float">: T.float32,
    "int">: T.int32,
    "integer">: T.bigint,
    "string">: T.string]

localBinding :: Binding -- ULocalBind
localBinding = define "LocalBinding" $
  -- omitted for now: fixity, pragma
  T.union [
    "signature">: typeSignature,
    "value">: valueBinding]

localBindings :: Binding -- ULocalBinds
localBindings = define "LocalBindings" $
  T.wrap $ T.list localBinding

module' :: Binding -- UModule
module' = define "Module" $
  -- omitted for now: pragma
  T.record [
    "head">: T.maybe moduleHead,
    "imports">: T.list import_,
    "declarations">: T.list declarationWithComments]

moduleHead :: Binding -- UModuleHead
moduleHead = define "ModuleHead" $
  -- omitted for now: pragma
  T.record [
    "comments">: T.maybe T.string,
    "name">: moduleName,
    "exports">: T.list export] -- UExportSpecs

moduleName :: Binding -- UModuleName
moduleName = define "ModuleName" $
  T.wrap T.string

name :: Binding -- UName
name = define "Name" $
  T.union [
    "implicit">: qualifiedName,
    "normal">: qualifiedName,
    "parens">: qualifiedName]

namePart :: Binding -- UNamePart
namePart = define "NamePart" $
  T.wrap T.string

operator :: Binding -- UOperator
operator = define "Operator" $
  T.union [
    "backtick">: qualifiedName,
    "normal">: qualifiedName]

pattern :: Binding -- UPattern
pattern = define "Pattern" $
  -- omitted for now: unboxed tuples, parallel arrays, irrefutable, bang, view, splice, quasiquote, plusk, unboxed sum
  T.union [
    "application">: applicationPattern,
    "as">: asPattern,
    "list">: T.list pattern,
    "literal">: literal,
    "name">: name,
    "parens">: pattern,
    "record">: recordPattern,
    "tuple">: T.list pattern,
    "typed">: typedPattern,
    "wildcard">: T.unit]

applicationPattern :: Binding
applicationPattern = define "ApplicationPattern" $
  T.record [
    "name">: name,
    "args">: T.list pattern]

asPattern :: Binding
asPattern = define "AsPattern" $
  T.record [
    "name">: name,
    "inner">: pattern]

recordPattern :: Binding
recordPattern = define "RecordPattern" $
  T.record [
    "name">: name,
    "fields">: T.list patternField]

typedPattern :: Binding
typedPattern = define "TypedPattern" $
  T.record [
    "inner">: pattern,
    "type">: type_]

patternField :: Binding -- UPatternField
patternField = define "PatternField" $
  -- omitted for now: puns, wildcards
  T.record [
    "name">: name,
    "pattern">: pattern]

qualifiedName :: Binding -- UQualifiedName
qualifiedName = define "QualifiedName" $
  T.record [
    "qualifiers">: T.list namePart,
    "unqualified">: namePart]

rightHandSide :: Binding -- URhs
rightHandSide = define "RightHandSide" $
  -- omitted for now: guarded rhs
  T.wrap expression

statement :: Binding -- UStmt
statement = define "Statement" $
  T.wrap expression

type_ :: Binding -- UType
type_ = define "Type" $
  -- omitted for now: forall, unboxed tuple, parallel array, kinded, promoted, splice, quasiquote, bang,
  --                  lazy, unpack, nounpack, wildcard, named wildcard, sum
  T.union [
    "application">: applicationType,
    "ctx">: contextType,
    "function">: functionType,
    "infix">: infixType,
    "list">: type_,
    "parens">: type_,
    "tuple">: T.list type_,
    "variable">: name]

applicationType :: Binding
applicationType = define "ApplicationType" $
  T.record [
    "context">: type_,
    "argument">: type_]

contextType :: Binding
contextType = define "ContextType" $
  T.record [
    "ctx">: assertion, -- UContext
    "type">: type_]

functionType :: Binding
functionType = define "FunctionType" $
  T.record [
    "domain">: type_,
    "codomain">: type_]

infixType :: Binding
infixType = define "InfixType" $
  T.record [
    "lhs">: type_,
    "operator">: operator,
    "rhs">: operator]

typeDeclaration :: Binding -- UTypeDecl
typeDeclaration = define "TypeDeclaration" $
  T.record [
    "name">: declarationHead,
    "type">: type_]

typeSignature :: Binding -- UTypeSignature
typeSignature = define "TypeSignature" $
  T.record [
    "name">: name,
    "type">: type_]

typedBinding :: Binding -- Added for convenience
typedBinding = define "TypedBinding" $
  T.record [
    "typeSignature">: typeSignature,
    "valueBinding">: valueBinding]

valueBinding :: Binding -- UValueBind
valueBinding = define "ValueBinding" $
  -- omitted for now: funBind
  T.union [
    "simple">: simpleValueBinding]

simpleValueBinding :: Binding
simpleValueBinding = define "SimpleValueBinding" $
  T.record [
    "pattern">: pattern,
    "rhs">: rightHandSide,
    "localBindings">: T.maybe localBindings]

variable :: Binding
variable = define "Variable" $
  -- omitted for now: kind constraints
  T.wrap name
