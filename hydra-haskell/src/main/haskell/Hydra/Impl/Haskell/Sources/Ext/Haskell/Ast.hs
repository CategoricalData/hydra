-- Note: this model is loosely based on Language.Haskell.Tools.AST.
--       References to corresponding symbols from that package can be found in comments.

module Hydra.Impl.Haskell.Sources.Ext.Haskell.Ast where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


haskellAstModule :: Module Meta
haskellAstModule = Module haskellAst []

haskellAstName :: GraphName
haskellAstName = GraphName "hydra/ext/haskell/ast"

haskellAst :: Graph Meta
haskellAst = Graph haskellAstName elements (const True) hydraCoreName
  where
    def = datatype haskellAstName
    ast = nominal . qualify haskellAstName . Name

    elements = [

      def "Alternative" $ -- UAlt
        doc "A pattern-matching alternative" $
        record [
          field "pattern" $ ast "Pattern",
          field "rhs" $ ast "CaseRhs",
          field "binds" $ optional $ ast "LocalBindings"],

      def "Assertion" $ -- UAssertion (UClassAssert)
        doc "A type assertion" $
        -- omitted for now: implicit and infix assertions
        record [
          field "name" $ ast "Name",
          field "types" $ list $ ast "Type"],

      def "CaseRhs" $ -- UCaseRhs'
        doc "The right-hand side of a pattern-matching alternative" $
        -- omitted for now: guarded
        ast "Expression",

      def "Constructor" $ -- UConDecl
        doc "A data constructor" $
        -- omitted for now: ordinary (positional), infix
        union [
          field "ordinary" $ ast "Constructor.Ordinary",
          field "record" $ ast "Constructor.Record"],

      def "Constructor.Ordinary" $
        doc "An ordinary (positional) data constructor" $
        record [
          field "name" $ ast "Name",
          field "fields" $ list $ ast "Type"],

      def "Constructor.Record" $
        doc "A record-style data constructor" $
        record [
          field "name" $ ast "Name",
          field "fields" $ list $ ast "Field"],

      def "DataDeclaration" $ -- UDataDecl
        doc "A data type declaration" $
        record [
          field "keyword" $ ast "DataDeclaration.Keyword",
          field "context" $ list $ ast "Assertion",
          field "head" $ ast "DeclarationHead",
          field "constructors" $ list $ ast "Constructor",
          field "deriving" $ list $ ast "Deriving"],

      def "DataDeclaration.Keyword" $
        doc "The 'data' versus 'newtype keyword" $
        enum ["data", "newtype"],

      def "DeclarationWithComments" $
        doc "A data declaration together with any comments" $
        record [
          field "body" $ ast "Declaration",
          field "comments" $ optional string],

      def "Declaration" $ -- UDecl
        doc "A data or value declaration" $
        -- omitted for now: typeFamily, typeSignature, closedTypeFamily, gDataDecl, typeInst, dataInst, gDataInst, class, inst,
        --                  patternSynonym, deriv, fixity, default, patTypeSig, foreignImport, foreignExport, pragma,
        --                  role, splice
        union [
          field "data" $ ast "DataDeclaration",
          field "type" $ ast "TypeDeclaration",
          field "valueBinding" $ ast "ValueBinding",
          field "typedBinding" $ ast "TypedBinding"],

      def "DeclarationHead" $ -- UDeclHead
        doc "The left-hand side of a declaration" $
        -- omitted for now: infix application
        union [
          field "application" $ ast "DeclarationHead.Application",
          field "parens" $ ast "DeclarationHead",
          field "simple" $ ast "Name"],

      def "DeclarationHead.Application" $
        doc "An application-style declaration head" $
        record [
          field "function" $ ast "DeclarationHead",
          field "operand" $ ast "Variable"],

      def "Deriving" $ -- UDeriving
        doc "A 'deriving' statement" $
        -- omitted for now: infix, parenthesized, and application instance heads
        list $ ast "Name",

      def "Export" $ -- UExportSpec
        doc "An export statement" $
        union [
          field "declaration" $ ast "ImportExportSpec",
          field "module" $ ast "ModuleName"],

      def "Expression" $ -- UExpr
        doc "A data expression" $
        -- omitted for now: multi-if, unboxed tuple, tuple section, unboxed tuple section, parallel array,
        --                  enum, parallel array enum, list comp, parallel array comp, type application,
        --                  (all Template Haskell constructors), pragma, arrow definition, arrow application,
        --                  lambda cases, static, unboxed sum, hole
        union [
          field "application" $ ast "Expression.Application",
          field "case" $ ast "Expression.Case",
          field "constructRecord" $ ast "Expression.ConstructRecord",
          field "do" $ list $ ast "Statement", -- omitted for now: do vs. mdo
          field "if" $ ast "Expression.If",
          field "infixApplication" $ ast "Expression.InfixApplication",
          field "literal" $ ast "Literal",
          field "lambda" $ ast "Expression.Lambda",
          field "leftSection" $ ast "Expression.Section",
          field "let" $ ast "Expression.Let",
          field "list" $ list $ ast "Expression",
          field "parens" $ ast "Expression",
          field "prefixApplication" $ ast "Expression.PrefixApplication",
          field "rightSection" $ ast "Expression.Section",
          field "tuple" $ list $ ast "Expression",
          field "typeSignature" $ ast "Expression.TypeSignature",
          field "updateRecord" $ ast "Expression.UpdateRecord",
          field "variable" $ ast "Name"],

      def "Expression.Application" $
        doc "An application expression" $
        record [
          field "function" $ ast "Expression",
          field "argument" $ ast "Expression"],

      def "Expression.Case" $
        doc "A case expression" $
        record [
          field "case" $ ast "Expression",
          field "alternatives" $ list $ ast "Alternative"],

      def "Expression.ConstructRecord" $
        doc "A record constructor expression" $
        record [
          field "name" $ ast "Name",
          field "fields" $ list $ ast "FieldUpdate"],

      def "Expression.If" $
        doc "An 'if' expression" $
        record [
          field "condition" $ ast "Expression",
          field "then" $ ast "Expression",
          field "else" $ ast "Expression"],

      def "Expression.InfixApplication" $
        doc "An infix application expression" $
        record [
          field "lhs" $ ast "Expression",
          field "operator" $ ast "Operator",
          field "rhs" $ ast "Expression"],

      def "Expression.Lambda" $
        doc "A lambda expression" $
        record [
          field "bindings" $ list $ ast "Pattern",
          field "inner" $ ast "Expression"],

      def "Expression.Let" $
        doc "A 'let' expression" $
        record [
          field "bindings" $ list $ ast "Pattern",
          field "inner" $ ast "Expression"],

      def "Expression.PrefixApplication" $
        doc "A prefix expression" $
        record [
          field "operator" $ ast "Operator",
          field "rhs" $ ast "Expression"],

      def "Expression.Section" $
        doc "A section expression" $
        record [
          field "operator" $ ast "Operator",
          field "expression" $ ast "Expression"],

      def "Expression.TypeSignature" $
        doc "A type signature expression" $
        record [
          field "inner" $ ast "Expression",
          field "type" $ ast "Type"],

      def "Expression.UpdateRecord" $
        doc "An update record expression" $
        record [
          field "inner" $ ast "Expression",
          field "fields" $ list $ ast "FieldUpdate"],

      def "Field" $ -- UFieldDecl
        doc "A field (name/type pair)" $
        record [
          field "name" $ ast "Name",
          field "type" $ ast "Type"],

      def "FieldUpdate" $ -- UFieldUpdate
        doc "A field name and value" $
        -- omitted for now: pun, wildcard
        record [
          field "name" $ ast "Name",
          field "value" $ ast "Expression"],

      def "Import" $ -- UImportDecl
        doc "An import statement" $
        -- omitted for now: source, safe, pkg
        record [
          field "qualified" boolean,
          field "module" $ ast "ModuleName",
          field "as" $ optional $ ast "ModuleName",
          field "spec" $ optional $ ast "Import.Spec"],

      def "Import.Spec" $
        doc "An import specification" $
        union [
          field "list" $ list $ ast "ImportExportSpec",
          field "hiding" $ list $ ast "ImportExportSpec"],

      def "ImportModifier" $ -- UImportModifier
        doc "An import modifier ('pattern' or 'type')" $
        enum ["pattern", "type"],

      def "ImportExportSpec" $ -- UIESpec
        doc "An import or export specification" $
        record [
          field "modifier" $ optional $ ast "ImportModifier",
          field "name" $ ast "Name",
          field "subspec" $ optional $ ast "ImportExportSpec.Subspec"],

      def "ImportExportSpec.Subspec" $
        union [
          field "all" unit,
          field "list" $ list $ ast "Name"],

      def "Literal" $ -- ULiteral
        doc "A literal value" $
        -- omitted for now: frac, primChar
        union [
          field "char" uint16,
          field "double" float64,
          field "float" float32,
          field "int" int32,
          field "integer" bigint,
          field "string" string],

      def "LocalBinding" $ -- ULocalBind
        -- omitted for now: fixity, pragma
        union [
          field "signature" $ ast "TypeSignature",
          field "value" $ ast "ValueBinding"],

      def "LocalBindings" $ -- ULocalBinds
        list $ ast "LocalBinding",

      def "Module" $ -- UModule
        -- omitted for now: pragma
        record [
          field "head" $ optional $ ast "ModuleHead",
          field "imports" $ list $ ast "Import",
          field "declarations" $ list $ ast "DeclarationWithComments"],

      def "ModuleHead" $ -- UModuleHead
        -- omitted for now: pragma
        record [
          field "name" $ ast "ModuleName",
          field "exports" $ list $ ast "Export"], -- UExportSpecs

      def "ModuleName" -- UModuleName
        string,

      def "Name" $ -- UName
        union [
          field "implicit" $ ast "QualifiedName",
          field "normal" $ ast "QualifiedName",
          field "parens" $ ast "QualifiedName"],

      def "NamePart" -- UNamePart
        string,

      def "Operator" $ -- UOperator
        union [
          field "backtick" $ ast "QualifiedName",
          field "normal" $ ast "QualifiedName"],

      def "Pattern" $ -- UPattern
        -- omitted for now: unboxed tuples, parallel arrays, irrefutable, bang, view, splice, quasiquote, plusk, unboxed sum
        union [
          field "application" $ ast "Pattern.Application",
          field "as" $ ast "Pattern.As",
          field "list" $ list $ ast "Pattern",
          field "literal" $ ast "Literal",
          field "name" $ ast "Name",
          field "parens" $ ast "Pattern",
          field "record" $ ast "Pattern.Record",
          field "tuple" $ list $ ast "Pattern",
          field "typed" $ ast "Pattern.Typed",
          field "wildcard" unit],

      def "Pattern.Application" $
        record [
          field "name" $ ast "Name",
          field "args" $ list $ ast "Pattern"],

      def "Pattern.As" $
        record [
          field "name" $ ast "Name",
          field "inner" $ ast "Pattern"],

      def "Pattern.Record" $
        record [
          field "name" $ ast "Name",
          field "fields" $ list $ ast "PatternField"],

      def "Pattern.Typed" $
        record [
          field "inner" $ ast "Pattern",
          field "type" $ ast "Type"],

      def "PatternField" $ -- UPatternField
        -- omitted for now: puns, wildcards
        record [
          field "name" $ ast "Name",
          field "pattern" $ ast "Pattern"],

      def "QualifiedName" $ -- UQualifiedName
        record [
          field "qualifiers" $ list $ ast "NamePart",
          field "unqualified" $ ast "NamePart"],

      def "RightHandSide" $ -- URhs
        -- omitted for now: guarded rhs
        ast "Expression",

      def "Statement" $ -- UStmt
        ast "Expression",

      def "Type" $ -- UType
        -- omitted for now: forall, ctx, unboxed tuple, parallel array, kinded, promoted, splice, quasiquote, bang,
        --                  lazy, unpack, nounpack, wildcard, named wildcard, sum
        union [
          field "application" $ ast "Type.Application",
          field "function" $ ast "Type.Function",
          field "infix" $ ast "Type.Infix",
          field "list" $ ast "Type",
          field "parens" $ ast "Type",
          field "tuple" $ list $ ast "Type",
          field "variable" $ ast "Name"],

      def "Type.Application" $
        record [
          field "context" $ ast "Type",
          field "argument" $ ast "Type"],

      def "Type.Function" $
        record [
          field "domain" $ ast "Type",
          field "codomain" $ ast "Type"],

      def "Type.Infix" $
        record [
          field "lhs" $ ast "Type",
          field "operator" $ ast "Operator",
          field "rhs" $ ast "Operator"],

      def "TypeDeclaration" $ -- UTypeDecl
        record [
          field "name" $ ast "DeclarationHead",
          field "type" $ ast "Type"],

      def "TypeSignature" $ -- UTypeSignature
        record [
          field "name" $ ast "Name",
          field "type" $ ast "Type"],

      def "TypedBinding" $ -- Added for convenience
        record [
          field "typeSignature" $ ast "TypeSignature",
          field "valueBinding" $ ast "ValueBinding"],

      def "ValueBinding" $ -- UValueBind
        -- omitted for now: funBind
        union [
          field "simple" $ ast "ValueBinding.Simple"],

      def "ValueBinding.Simple" $
        record [
          field "pattern" $ ast "Pattern",
          field "rhs" $ ast "RightHandSide",
          field "localBindings" $ optional $ ast "LocalBindings"],
          
      def "Variable" $
        -- omitted for now: kind constraints
        ast "Name"]
