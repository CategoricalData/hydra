{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Haskell.Serde where

-- Standard Tier-3 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Ast                       as Ast
import qualified Hydra.Dsl.Coders                    as Coders
import qualified Hydra.Dsl.Compute                   as Compute
import qualified Hydra.Dsl.Core                      as Core
import qualified Hydra.Dsl.Graph                     as Graph
import qualified Hydra.Dsl.Lib.Chars                 as Chars
import qualified Hydra.Dsl.Lib.Equality              as Equality
import qualified Hydra.Dsl.Lib.Flows                 as Flows
import qualified Hydra.Dsl.Lib.Lists                 as Lists
import qualified Hydra.Dsl.Lib.Literals              as Literals
import qualified Hydra.Dsl.Lib.Logic                 as Logic
import qualified Hydra.Dsl.Lib.Maps                  as Maps
import qualified Hydra.Dsl.Lib.Math                  as Math
import qualified Hydra.Dsl.Lib.Optionals             as Optionals
import           Hydra.Dsl.Phantoms                  as Phantoms
import qualified Hydra.Dsl.Lib.Sets                  as Sets
import qualified Hydra.Dsl.Lib.Strings               as Strings
import qualified Hydra.Dsl.Mantle                    as Mantle
import qualified Hydra.Dsl.Module                    as Module
import qualified Hydra.Dsl.TTerms                    as TTerms
import qualified Hydra.Dsl.TTypes                    as TTypes
import qualified Hydra.Dsl.Terms                     as Terms
import qualified Hydra.Dsl.Topology                  as Topology
import qualified Hydra.Dsl.Types                     as Types
import qualified Hydra.Dsl.Typing                    as Typing
import qualified Hydra.Sources.Tier1.All             as Tier1
import qualified Hydra.Sources.Tier1.Constants       as Constants
import qualified Hydra.Sources.Tier1.Decode          as Decode
import qualified Hydra.Sources.Tier1.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Tier1.Formatting      as Formatting
import qualified Hydra.Sources.Tier1.Functions       as Functions
import qualified Hydra.Sources.Tier1.Literals        as Literals
import qualified Hydra.Sources.Tier1.Messages        as Messages
import qualified Hydra.Sources.Tier1.Strip           as Strip
import qualified Hydra.Sources.Tier2.Accessors       as Accessors
import qualified Hydra.Sources.Tier2.AdapterUtils    as AdapterUtils
import qualified Hydra.Sources.Tier2.Adapters        as Adapters
import qualified Hydra.Sources.Tier2.Annotations     as Annotations
import qualified Hydra.Sources.Tier2.Arity           as Arity
import qualified Hydra.Sources.Tier2.CoreLanguage    as CoreLanguage
import qualified Hydra.Sources.Tier2.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Tier2.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Tier2.Errors          as Errors
import qualified Hydra.Sources.Tier2.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Tier2.Monads           as Flows_
import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
import qualified Hydra.Sources.Tier2.Inference       as Inference
import qualified Hydra.Sources.Tier2.Lexical         as Lexical
import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
import qualified Hydra.Sources.Tier2.Qnames          as Qnames
import qualified Hydra.Sources.Tier2.Reduction       as Reduction
import qualified Hydra.Sources.Tier2.Rewriting       as Rewriting
import qualified Hydra.Sources.Tier2.Schemas         as Schemas
import qualified Hydra.Sources.Tier2.Serialization   as Serialization
import qualified Hydra.Sources.Tier2.Show.Core       as ShowCore
import qualified Hydra.Sources.Tier2.Sorting         as Sorting
import qualified Hydra.Sources.Tier2.Substitution    as Substitution
import qualified Hydra.Sources.Tier2.Tarjan          as Tarjan
import qualified Hydra.Sources.Tier2.Templating      as Templating
import qualified Hydra.Sources.Tier2.TermAdapters    as TermAdapters
import qualified Hydra.Sources.Tier2.Unification     as Unification
import qualified Hydra.Sources.Tier2.Variants        as Variants
import qualified Data.Int                            as I
import qualified Data.List                           as L
import qualified Data.Map                            as M
import qualified Data.Set                            as S
import qualified Data.Maybe                          as Y

import Hydra.Ast
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Sources.Tier3.Ext.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Tier3.Ext.Haskell.Operators as Operators


haskellSerdeDefinition :: String -> TTerm a -> TElement a
haskellSerdeDefinition = definitionInModule haskellSerdeModule

haskellSerdeModule :: Module
haskellSerdeModule = Module ns elements
    [Serialization.hydraSerializationModule, Operators.haskellOperatorsModule]
    [Tier1.hydraAstModule, HaskellAst.haskellAstModule, Tier1.hydraGraphModule] $
    Just ("Haskell operator precendence and associativity are drawn from:\n"
      <> "https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html\n"
      <> "Other operators were investigated using GHCi, e.g. \":info (->)\"\n"
      <> "Operator names are drawn (loosely) from:\n"
      <> "https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators")
  where
    ns = Namespace "hydra.ext.haskell.serde"
    elements = [
      el alternativeToExprDef,
      el applicationExpressionToExprDef,
      el applicationPatternToExprDef,
      el assertionToExprDef,
      el caseExpressionToExprDef,
      el caseRhsToExprDef,
      el classAssertionToExprDef,
      el constructorToExprDef,
      el constructorWithCommentsToExprDef,
      el dataOrNewtypeToExprDef,
      el declarationHeadToExprDef,
      el declarationToExprDef,
      el declarationWithCommentsToExprDef,
      el expressionToExprDef,
      el constructRecordExpressionToExprDef,
      el fieldToExprDef,
      el fieldWithCommentsToExprDef,
      el ifExpressionToExprDef,
      el importExportSpecToExprDef,
      el importToExprDef,
      el lambdaExpressionToExprDef,
      el literalToExprDef,
      el localBindingToExprDef,
      el moduleHeadToExprDef,
      el moduleToExprDef,
      el nameToExprDef,
      el patternToExprDef,
      el rightHandSideToExprDef,
      el statementToExprDef,
      el typeSignatureToExprDef,
      el typeToExprDef,
      el valueBindingToExprDef,
      el variableToExprDef,
      el toHaskellCommentsDef,
      el writeQualifiedNameDef]

alternativeToExprDef :: TElement (H.Alternative -> Expr)
alternativeToExprDef = haskellSerdeDefinition "alternativeToExpr" $
  lambda "alt" $ 
    ref Serialization.ifxDef @@ ref Operators.caseOpDef @@
      (ref patternToExprDef @@ (project H._Alternative H._Alternative_pattern @@ var "alt")) @@
      (ref caseRhsToExprDef @@ (project H._Alternative H._Alternative_rhs @@ var "alt"))

applicationExpressionToExprDef :: TElement (H.ApplicationExpression -> Expr)
applicationExpressionToExprDef = haskellSerdeDefinition "applicationExpressionToExpr" $
  lambda "app" $
    ref Serialization.ifxDef @@ ref Operators.appOpDef @@
      (ref expressionToExprDef @@ (project H._ApplicationExpression H._ApplicationExpression_function @@ var "app")) @@
      (ref expressionToExprDef @@ (project H._ApplicationExpression H._ApplicationExpression_argument @@ var "app"))

applicationPatternToExprDef :: TElement (H.ApplicationPattern -> Expr)
applicationPatternToExprDef = haskellSerdeDefinition "applicationPatternToExpr" $
  lambda "appPat" $ lets [
            "name">: project H._ApplicationPattern H._ApplicationPattern_name @@ var "appPat",
    "pats">: project H._ApplicationPattern H._ApplicationPattern_args @@ var "appPat"] $
    ref Serialization.spaceSepDef @@ (Lists.cons (ref nameToExprDef @@ var "name") (Lists.map (ref patternToExprDef) (var "pats")))

assertionToExprDef :: TElement (H.Assertion -> Expr)
assertionToExprDef = haskellSerdeDefinition "assertionToExpr" $
  lambda "sert" $
    cases H._Assertion (var "sert") Nothing [
      H._Assertion_class>>: lambda "cls" $ ref classAssertionToExprDef @@ var "cls",
      H._Assertion_tuple>>: lambda "serts" $
        ref Serialization.parenListDef @@ false @@ (Lists.map (ref assertionToExprDef) (var "serts"))]

caseExpressionToExprDef :: TElement (H.CaseExpression -> Expr)
caseExpressionToExprDef = haskellSerdeDefinition "caseExpressionToExpr" $
  lambda "caseExpr" $ lets [
    "cs">: project H._CaseExpression H._CaseExpression_case @@ var "caseExpr",
    "alts">: project H._CaseExpression H._CaseExpression_alternatives @@ var "caseExpr",
    "ofOp">: Ast.op
      (Ast.symbol $ string "of")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone,
    "lhs">: ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "case", ref expressionToExprDef @@ var "cs"],
    "rhs">: ref Serialization.newlineSepDef @@ (Lists.map (ref alternativeToExprDef) (var "alts"))] $
    ref Serialization.ifxDef @@ var "ofOp" @@ var "lhs" @@ var "rhs"

caseRhsToExprDef :: TElement (H.CaseRhs -> Expr)
caseRhsToExprDef = haskellSerdeDefinition "caseRhsToExpr" $
  lambda "rhs" $ ref expressionToExprDef @@ (unwrap H._CaseRhs @@ var "rhs")

classAssertionToExprDef :: TElement (H.ClassAssertion -> Expr)
classAssertionToExprDef = haskellSerdeDefinition "classAssertionToExpr" $
  lambda "clsAsrt" $ lets [
    "name">: project H._ClassAssertion H._ClassAssertion_name @@ var "clsAsrt",
    "types">: project H._ClassAssertion H._ClassAssertion_types @@ var "clsAsrt"] $
    ref Serialization.spaceSepDef @@ list [
      ref nameToExprDef @@ var "name",
      ref Serialization.commaSepDef @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (ref typeToExprDef) (var "types"))]

constructorToExprDef :: TElement (H.Constructor -> Expr)
constructorToExprDef = haskellSerdeDefinition "constructorToExpr" $
  lambda "cons" $
    cases H._Constructor (var "cons") Nothing [
      H._Constructor_ordinary>>: lambda "ord" $ lets [
        "name">: project H._OrdinaryConstructor H._OrdinaryConstructor_name @@ var "ord",
        "types">: project H._OrdinaryConstructor H._OrdinaryConstructor_fields @@ var "ord"] $
        ref Serialization.spaceSepDef @@ list [ref nameToExprDef @@ var "name", ref Serialization.spaceSepDef @@ (Lists.map (ref typeToExprDef) (var "types"))],
      H._Constructor_record>>: lambda "rec" $ lets [
        "name">: project H._RecordConstructor H._RecordConstructor_name @@ var "rec",
        "fields">: project H._RecordConstructor H._RecordConstructor_fields @@ var "rec"] $
        ref Serialization.spaceSepDef @@ list [
          ref nameToExprDef @@ var "name",
          ref Serialization.curlyBracesListDef @@ nothing @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (ref fieldWithCommentsToExprDef) (var "fields"))]]

constructorWithCommentsToExprDef :: TElement (H.ConstructorWithComments -> Expr)
constructorWithCommentsToExprDef = haskellSerdeDefinition "constructorWithCommentsToExpr" $
  lambda "consWithComments" $ lets [
    "body">: project H._ConstructorWithComments H._ConstructorWithComments_body @@ var "consWithComments",
    "mc">: project H._ConstructorWithComments H._ConstructorWithComments_comments @@ var "consWithComments"] $
    Optionals.maybe
      (ref constructorToExprDef @@ var "body")
      (lambda "c" $ ref Serialization.newlineSepDef @@ list [
        ref Serialization.cstDef @@ (ref toHaskellCommentsDef @@ var "c"),
        ref constructorToExprDef @@ var "body"])
      (var "mc")

dataOrNewtypeToExprDef :: TElement (H.DataOrNewtype -> Expr)
dataOrNewtypeToExprDef = haskellSerdeDefinition "dataOrNewtypeToExpr" $
  lambda "kw" $
    cases H._DataOrNewtype (var "kw") Nothing [
      H._DataOrNewtype_data>>: constant $ ref Serialization.cstDef @@ string "data",
      H._DataOrNewtype_newtype>>: constant $ ref Serialization.cstDef @@ string "newtype"]

declarationHeadToExprDef :: TElement (H.DeclarationHead -> Expr)
declarationHeadToExprDef = haskellSerdeDefinition "declarationHeadToExpr" $
  lambda "hd" $
    cases H._DeclarationHead (var "hd") Nothing [
      H._DeclarationHead_application>>: lambda "appHead" $ lets [
        "fun">: project H._ApplicationDeclarationHead H._ApplicationDeclarationHead_function @@ var "appHead",
        "op">: project H._ApplicationDeclarationHead H._ApplicationDeclarationHead_operand @@ var "appHead"] $
        ref Serialization.spaceSepDef @@ list [ref declarationHeadToExprDef @@ var "fun", ref variableToExprDef @@ var "op"],
      H._DeclarationHead_simple>>: lambda "name" $ ref nameToExprDef @@ var "name"]

declarationToExprDef :: TElement (H.Declaration -> Expr)
declarationToExprDef = haskellSerdeDefinition "declarationToExpr" $
  lambda "decl" $
    cases H._Declaration (var "decl") Nothing [
      H._Declaration_data>>: lambda "dataDecl" $ lets [
        "kw">: project H._DataDeclaration H._DataDeclaration_keyword @@ var "dataDecl",
        "hd">: project H._DataDeclaration H._DataDeclaration_head @@ var "dataDecl",
        "cons">: project H._DataDeclaration H._DataDeclaration_constructors @@ var "dataDecl",
        "deriv">: project H._DataDeclaration H._DataDeclaration_deriving @@ var "dataDecl",
        "derivCat">: Lists.concat $ Lists.map (unwrap H._Deriving) (var "deriv"),
        "constructors">: ref Serialization.orSepDef @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (ref constructorWithCommentsToExprDef) (var "cons")),
        "derivingClause">: Logic.ifElse (Lists.null $ var "derivCat")
          (list [])
          (list [ref Serialization.spaceSepDef @@ list [
            ref Serialization.cstDef @@ string "deriving",
            ref Serialization.parenListDef @@ false @@ (Lists.map (ref nameToExprDef) (var "derivCat"))]]),
        "mainParts">: list [
          ref Serialization.spaceSepDef @@ list [ref dataOrNewtypeToExprDef @@ var "kw", ref declarationHeadToExprDef @@ var "hd", ref Serialization.cstDef @@ string "="],
          var "constructors"]] $
        ref Serialization.indentBlockDef @@ Lists.concat2 (var "mainParts") (var "derivingClause"),
      H._Declaration_type>>: lambda "typeDecl" $ lets [
        "hd">: project H._TypeDeclaration H._TypeDeclaration_name @@ var "typeDecl",
        "typ">: project H._TypeDeclaration H._TypeDeclaration_type @@ var "typeDecl"] $
        ref Serialization.spaceSepDef @@ list [
          ref Serialization.cstDef @@ string "type",
          ref declarationHeadToExprDef @@ var "hd",
          ref Serialization.cstDef @@ string "=",
          ref typeToExprDef @@ var "typ"],
      H._Declaration_valueBinding>>: lambda "vb" $ ref valueBindingToExprDef @@ var "vb",
      H._Declaration_typedBinding>>: lambda "typedBinding" $ lets [
        "typeSig">: project H._TypedBinding H._TypedBinding_typeSignature @@ var "typedBinding",
        "vb">: project H._TypedBinding H._TypedBinding_valueBinding @@ var "typedBinding",
        "name">: project H._TypeSignature H._TypeSignature_name @@ var "typeSig",
        "htype">: project H._TypeSignature H._TypeSignature_type @@ var "typeSig"] $
        ref Serialization.newlineSepDef @@ list [
          ref Serialization.ifxDef @@ ref Operators.typeOpDef @@ (ref nameToExprDef @@ var "name") @@ (ref typeToExprDef @@ var "htype"),
          ref valueBindingToExprDef @@ var "vb"]]

declarationWithCommentsToExprDef :: TElement (H.DeclarationWithComments -> Expr)
declarationWithCommentsToExprDef = haskellSerdeDefinition "declarationWithCommentsToExpr" $
  lambda "declWithComments" $ lets [
    "body">: project H._DeclarationWithComments H._DeclarationWithComments_body @@ var "declWithComments",
    "mc">: project H._DeclarationWithComments H._DeclarationWithComments_comments @@ var "declWithComments"] $
    Optionals.maybe
      (ref declarationToExprDef @@ var "body")
      (lambda "c" $ ref Serialization.newlineSepDef @@ list [
        ref Serialization.cstDef @@ (ref toHaskellCommentsDef @@ var "c"),
        ref declarationToExprDef @@ var "body"])
      (var "mc")

expressionToExprDef :: TElement (H.Expression -> Expr)
expressionToExprDef = haskellSerdeDefinition "expressionToExpr" $
  lambda "expr" $
    cases H._Expression (var "expr") Nothing [
      H._Expression_application>>: lambda "app" $ ref applicationExpressionToExprDef @@ var "app",
      H._Expression_case>>: lambda "cases" $ ref caseExpressionToExprDef @@ var "cases",
      H._Expression_constructRecord>>: lambda "r" $ ref constructRecordExpressionToExprDef @@ var "r",
      H._Expression_do>>: lambda "statements" $
        ref Serialization.indentBlockDef @@ Lists.cons (ref Serialization.cstDef @@ string "do") (Lists.map (ref statementToExprDef) (var "statements")),
      H._Expression_if>>: lambda "ifte" $ ref ifExpressionToExprDef @@ var "ifte",
      H._Expression_literal>>: lambda "lit" $ ref literalToExprDef @@ var "lit",
      H._Expression_lambda>>: lambda "lam" $ ref Serialization.parenthesizeDef @@ (ref lambdaExpressionToExprDef @@ var "lam"),
      H._Expression_let>>: lambda "letExpr" $ lets [
        "bindings">: project H._LetExpression H._LetExpression_bindings @@ var "letExpr",
        "inner">: project H._LetExpression H._LetExpression_inner @@ var "letExpr",
        "encodeBinding">: lambda "binding" $
          ref Serialization.indentSubsequentLinesDef @@ string "      " @@ (ref localBindingToExprDef @@ var "binding")] $
        ref Serialization.indentBlockDef @@ list [
          ref Serialization.cstDef @@ string "",
          ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "let", ref Serialization.customIndentBlockDef @@ string "    " @@ (Lists.map (var "encodeBinding") (var "bindings"))],
          ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "in", ref expressionToExprDef @@ var "inner"]],
      H._Expression_list>>: lambda "exprs" $
        ref Serialization.bracketListDef @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (ref expressionToExprDef) (var "exprs")),
      H._Expression_parens>>: lambda "expr'" $ ref Serialization.parenthesizeDef @@ (ref expressionToExprDef @@ var "expr'"),
      H._Expression_tuple>>: lambda "exprs" $
        ref Serialization.parenListDef @@ false @@ (Lists.map (ref expressionToExprDef) (var "exprs")),
      H._Expression_variable>>: lambda "name" $ ref nameToExprDef @@ var "name"]

constructRecordExpressionToExprDef :: TElement (H.ConstructRecordExpression -> Expr)
constructRecordExpressionToExprDef = haskellSerdeDefinition "constructRecordExpressionToExpr" $
  lambda "constructRecord" $ lets [
    "name">: project H._ConstructRecordExpression H._ConstructRecordExpression_name @@ var "constructRecord",
    "updates">: project H._ConstructRecordExpression H._ConstructRecordExpression_fields @@ var "constructRecord",
    "fromUpdate">: lambda "update" $ lets [
      "fn">: project H._FieldUpdate H._FieldUpdate_name @@ var "update",
      "val">: project H._FieldUpdate H._FieldUpdate_value @@ var "update"] $
      ref Serialization.ifxDef @@ ref Operators.defineOpDef @@ (ref nameToExprDef @@ var "fn") @@ (ref expressionToExprDef @@ var "val"),
    "body">: ref Serialization.commaSepDef @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (var "fromUpdate") (var "updates"))] $
    ref Serialization.spaceSepDef @@ list [
      ref nameToExprDef @@ var "name",
      ref Serialization.bracketsDef @@ ref Serialization.curlyBracesDef @@ ref Serialization.halfBlockStyleDef @@ var "body"]

fieldToExprDef :: TElement (H.Field -> Expr)
fieldToExprDef = haskellSerdeDefinition "fieldToExpr" $
  lambda "field" $ lets [
    "name">: project H._Field H._Field_name @@ var "field",
    "typ">: project H._Field H._Field_type @@ var "field"] $
    ref Serialization.spaceSepDef @@ list [ref nameToExprDef @@ var "name", ref Serialization.cstDef @@ string "::", ref typeToExprDef @@ var "typ"]

fieldWithCommentsToExprDef :: TElement (H.FieldWithComments -> Expr)
fieldWithCommentsToExprDef = haskellSerdeDefinition "fieldWithCommentsToExpr" $
  lambda "fieldWithComments" $ lets [
    "field">: project H._FieldWithComments H._FieldWithComments_field @@ var "fieldWithComments",
    "mc">: project H._FieldWithComments H._FieldWithComments_comments @@ var "fieldWithComments"] $
    Optionals.maybe
      (ref fieldToExprDef @@ var "field")
      (lambda "c" $ ref Serialization.newlineSepDef @@ list [
        ref Serialization.cstDef @@ (ref toHaskellCommentsDef @@ var "c"),
        ref fieldToExprDef @@ var "field"])
      (var "mc")

ifExpressionToExprDef :: TElement (H.IfExpression -> Expr)
ifExpressionToExprDef = haskellSerdeDefinition "ifExpressionToExpr" $
  lambda "ifExpr" $ lets [
    "eif">: project H._IfExpression H._IfExpression_condition @@ var "ifExpr",
    "ethen">: project H._IfExpression H._IfExpression_then @@ var "ifExpr",
    "eelse">: project H._IfExpression H._IfExpression_else @@ var "ifExpr",
    "ifOp">: Ast.op
      (Ast.symbol $ string "")
      (Ast.padding Ast.wsNone (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone,
    "body">: ref Serialization.newlineSepDef @@ list [
      ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "then", ref expressionToExprDef @@ var "ethen"],
      ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "else", ref expressionToExprDef @@ var "eelse"]]] $
    ref Serialization.ifxDef @@ var "ifOp" @@
      (ref Serialization.spaceSepDef @@ list [ref Serialization.cstDef @@ string "if", ref expressionToExprDef @@ var "eif"]) @@
      var "body"

importExportSpecToExprDef :: TElement (H.ImportExportSpec -> Expr)
importExportSpecToExprDef = haskellSerdeDefinition "importExportSpecToExpr" $
  lambda "spec" $ ref nameToExprDef @@ (project H._ImportExportSpec H._ImportExportSpec_name @@ var "spec")

importToExprDef :: TElement (H.Import -> Expr)
importToExprDef = haskellSerdeDefinition "importToExpr" $
  lambda "import" $ lets [
    "qual">: project H._Import H._Import_qualified @@ var "import",
    "modName">: project H._Import H._Import_module @@ var "import",
    "mod">: project H._Import H._Import_as @@ var "import",
    "mspec">: project H._Import H._Import_spec @@ var "import",
    "name">: unwrap H._ModuleName @@ var "modName",
    "hidingSec">: lambda "spec" $
      cases H._SpecImport (var "spec") Nothing [
        H._SpecImport_hiding>>: lambda "names" $
          ref Serialization.spaceSepDef @@ list [
            ref Serialization.cstDef @@ string "hiding ",
            ref Serialization.parensDef @@
              (ref Serialization.commaSepDef @@ ref Serialization.inlineStyleDef @@ (Lists.map (ref importExportSpecToExprDef) (var "names")))]],
    "parts">: Optionals.cat $ list [
      just $ ref Serialization.cstDef @@ string "import",
      Logic.ifElse (var "qual") (just $ ref Serialization.cstDef @@ string "qualified") nothing,
      just $ ref Serialization.cstDef @@ var "name",
      Optionals.map (lambda "m" $ ref Serialization.cstDef @@ Strings.cat2 (string "as ") (unwrap H._ModuleName @@ var "m")) (var "mod"),
      Optionals.map (var "hidingSec") (var "mspec")]] $
    ref Serialization.spaceSepDef @@ var "parts"

lambdaExpressionToExprDef :: TElement (H.LambdaExpression -> Expr)
lambdaExpressionToExprDef = haskellSerdeDefinition "lambdaExpressionToExpr" $
  lambda "lambdaExpr" $ lets [
    "bindings">: project H._LambdaExpression H._LambdaExpression_bindings @@ var "lambdaExpr",
            "inner">: project H._LambdaExpression H._LambdaExpression_inner @@ var "lambdaExpr",
    "head">: ref Serialization.spaceSepDef @@ (Lists.map (ref patternToExprDef) (var "bindings")),
    "body">: ref expressionToExprDef @@ var "inner"] $
    ref Serialization.ifxDef @@ ref Operators.lambdaOpDef @@
      (ref Serialization.prefixDef @@ string "\\" @@ var "head") @@
      var "body"

literalToExprDef :: TElement (H.Literal -> Expr)
literalToExprDef = haskellSerdeDefinition "literalToExpr" $
  lambda "lit" $
    ref Serialization.cstDef @@
      cases H._Literal (var "lit") Nothing [
        H._Literal_char>>: lambda "c" $ Literals.showString $ Literals.showUint16 $ var "c", -- Simplified char handling
        H._Literal_double>>: lambda "d" $
          Logic.ifElse (Equality.ltFloat64 (var "d") (float64 0.0))
            (Strings.cat2 (string "(0") (Strings.cat2 (Literals.showFloat64 $ var "d") (string ")")))
            (Literals.showFloat64 $ var "d"),
        H._Literal_float>>: lambda "f" $
          Logic.ifElse (Equality.ltFloat32 (var "f") (float32 0.0))
            (Strings.cat2 (string "(0") (Strings.cat2 (Literals.showFloat32 $ var "f") (string ")")))
            (Literals.showFloat32 $ var "f"),
        H._Literal_int>>: lambda "i" $
          Logic.ifElse (Equality.ltInt32 (var "i") (int32 0))
            (Strings.cat2 (string "(0") (Strings.cat2 (Literals.showInt32 $ var "i") (string ")")))
            (Literals.showInt32 $ var "i"),
        H._Literal_integer>>: lambda "i" $ Literals.showBigint $ var "i",
        H._Literal_string>>: lambda "s" $ Literals.showString $ var "s"]

localBindingToExprDef :: TElement (H.LocalBinding -> Expr)
localBindingToExprDef = haskellSerdeDefinition "localBindingToExpr" $
  lambda "binding" $
    cases H._LocalBinding (var "binding") Nothing [
      H._LocalBinding_signature>>: lambda "ts" $ ref typeSignatureToExprDef @@ var "ts",
      H._LocalBinding_value>>: lambda "vb" $ ref valueBindingToExprDef @@ var "vb"]

moduleHeadToExprDef :: TElement (H.ModuleHead -> Expr)
moduleHeadToExprDef = haskellSerdeDefinition "moduleHeadToExpr" $
  lambda "moduleHead" $ lets [
    "mc">: project H._ModuleHead H._ModuleHead_comments @@ var "moduleHead",
    "modName">: project H._ModuleHead H._ModuleHead_name @@ var "moduleHead",
    "mname">: unwrap H._ModuleName @@ var "modName",
    "head">: ref Serialization.spaceSepDef @@ list [
      ref Serialization.cstDef @@ string "module",
      ref Serialization.cstDef @@ var "mname",
      ref Serialization.cstDef @@ string "where"]] $
    Optionals.maybe
      (var "head")
      (lambda "c" $ ref Serialization.newlineSepDef @@ list [
        ref Serialization.cstDef @@ (ref toHaskellCommentsDef @@ var "c"),
        ref Serialization.cstDef @@ string "",
        var "head"])
      (var "mc")

moduleToExprDef :: TElement (H.Module -> Expr)
moduleToExprDef = haskellSerdeDefinition "moduleToExpr" $
  lambda "module" $ lets [
    "mh">: project H._Module H._Module_head @@ var "module",
    "imports">: project H._Module H._Module_imports @@ var "module",
    "decls">: project H._Module H._Module_declarations @@ var "module",
    "headerLine">: Optionals.maybe (list []) (lambda "h" $ list [ref moduleHeadToExprDef @@ var "h"]) (var "mh"),
    "declLines">: Lists.map (ref declarationWithCommentsToExprDef) (var "decls"),
    "importLines">: Logic.ifElse (Lists.null $ var "imports")
      (list [])
      (list [ref Serialization.newlineSepDef @@ (Lists.map (ref importToExprDef) (var "imports"))])] $
    ref Serialization.doubleNewlineSepDef @@ (Lists.concat $ list [var "headerLine", var "importLines", var "declLines"])

nameToExprDef :: TElement (H.Name -> Expr)
nameToExprDef = haskellSerdeDefinition "nameToExpr" $
  lambda "name" $
    ref Serialization.cstDef @@
      cases H._Name (var "name") Nothing [
        H._Name_implicit>>: lambda "qn" $ Strings.cat2 (string "?") (ref writeQualifiedNameDef @@ var "qn"),
        H._Name_normal>>: lambda "qn" $ ref writeQualifiedNameDef @@ var "qn",
        H._Name_parens>>: lambda "qn" $ Strings.cat $ list [string "(", ref writeQualifiedNameDef @@ var "qn", string ")"]]

patternToExprDef :: TElement (H.Pattern -> Expr)
patternToExprDef = haskellSerdeDefinition "patternToExpr" $
  lambda "pat" $
    cases H._Pattern (var "pat") Nothing [
      H._Pattern_application>>: lambda "app" $ ref applicationPatternToExprDef @@ var "app",
      H._Pattern_list>>: lambda "pats" $
        ref Serialization.bracketListDef @@ ref Serialization.halfBlockStyleDef @@ (Lists.map (ref patternToExprDef) (var "pats")),
      H._Pattern_literal>>: lambda "lit" $ ref literalToExprDef @@ var "lit",
      H._Pattern_name>>: lambda "name" $ ref nameToExprDef @@ var "name",
      H._Pattern_parens>>: lambda "pat'" $ ref Serialization.parenthesizeDef @@ (ref patternToExprDef @@ var "pat'"),
      H._Pattern_tuple>>: lambda "pats" $
        ref Serialization.parenListDef @@ false @@ (Lists.map (ref patternToExprDef) (var "pats")),
      H._Pattern_wildcard>>: constant $ ref Serialization.cstDef @@ string "_"]

rightHandSideToExprDef :: TElement (H.RightHandSide -> Expr)
rightHandSideToExprDef = haskellSerdeDefinition "rightHandSideToExpr" $
  lambda "rhs" $ ref expressionToExprDef @@ (unwrap H._RightHandSide @@ var "rhs")

statementToExprDef :: TElement (H.Statement -> Expr)
statementToExprDef = haskellSerdeDefinition "statementToExpr" $
  lambda "stmt" $ ref expressionToExprDef @@ (unwrap H._Statement @@ var "stmt")

typeSignatureToExprDef :: TElement (H.TypeSignature -> Expr)
typeSignatureToExprDef = haskellSerdeDefinition "typeSignatureToExpr" $
  lambda "typeSig" $ lets [
    "name">: project H._TypeSignature H._TypeSignature_name @@ var "typeSig",
    "typ">: project H._TypeSignature H._TypeSignature_type @@ var "typeSig"] $
    ref Serialization.spaceSepDef @@ list [ref nameToExprDef @@ var "name", ref Serialization.cstDef @@ string "::", ref typeToExprDef @@ var "typ"]

typeToExprDef :: TElement (H.Type -> Expr)
typeToExprDef = haskellSerdeDefinition "typeToExpr" $
  lambda "htype" $
    cases H._Type (var "htype") Nothing [
      H._Type_application>>: lambda "appType" $ lets [
        "lhs">: project H._ApplicationType H._ApplicationType_context @@ var "appType",
        "rhs">: project H._ApplicationType H._ApplicationType_argument @@ var "appType"] $
        ref Serialization.ifxDef @@ ref Operators.appOpDef @@ (ref typeToExprDef @@ var "lhs") @@ (ref typeToExprDef @@ var "rhs"),
      H._Type_ctx>>: lambda "ctxType" $ lets [
        "ctx">: project H._ContextType H._ContextType_ctx @@ var "ctxType",
        "typ">: project H._ContextType H._ContextType_type @@ var "ctxType"] $
        ref Serialization.ifxDef @@ ref Operators.assertOpDef @@ (ref assertionToExprDef @@ var "ctx") @@ (ref typeToExprDef @@ var "typ"),
      H._Type_function>>: lambda "funType" $ lets [
        "dom">: project H._FunctionType H._FunctionType_domain @@ var "funType",
        "cod">: project H._FunctionType H._FunctionType_codomain @@ var "funType"] $
        ref Serialization.ifxDef @@ ref Operators.arrowOpDef @@ (ref typeToExprDef @@ var "dom") @@ (ref typeToExprDef @@ var "cod"),
      H._Type_list>>: lambda "htype'" $
        ref Serialization.bracketListDef @@ ref Serialization.inlineStyleDef @@ list [ref typeToExprDef @@ var "htype'"],
      H._Type_tuple>>: lambda "types" $
        ref Serialization.parenListDef @@ false @@ (Lists.map (ref typeToExprDef) (var "types")),
      H._Type_variable>>: lambda "name" $ ref nameToExprDef @@ var "name"]

valueBindingToExprDef :: TElement (H.ValueBinding -> Expr)
valueBindingToExprDef = haskellSerdeDefinition "valueBindingToExpr" $
  lambda "vb" $
    cases H._ValueBinding (var "vb") Nothing [
      H._ValueBinding_simple>>: lambda "simpleVB" $ lets [
        "pat">: project H._SimpleValueBinding H._SimpleValueBinding_pattern @@ var "simpleVB",
        "rhs">: project H._SimpleValueBinding H._SimpleValueBinding_rhs @@ var "simpleVB",
        "local">: project H._SimpleValueBinding H._SimpleValueBinding_localBindings @@ var "simpleVB",
        "body">: ref Serialization.ifxDef @@ ref Operators.defineOpDef @@ (ref patternToExprDef @@ var "pat") @@ (ref rightHandSideToExprDef @@ var "rhs")] $
        Optionals.maybe
          (var "body")
          (lambda "localBindings" $ lets [
            "bindings">: unwrap H._LocalBindings @@ var "localBindings"] $
            ref Serialization.indentBlockDef @@ list [
              var "body",
              ref Serialization.indentBlockDef @@ Lists.cons (ref Serialization.cstDef @@ string "where") (Lists.map (ref localBindingToExprDef) (var "bindings"))])
          (var "local")]

variableToExprDef :: TElement (H.Variable -> Expr)
variableToExprDef = haskellSerdeDefinition "variableToExpr" $
  lambda "variable" $ ref nameToExprDef @@ (unwrap H._Variable @@ var "variable")

toHaskellCommentsDef :: TElement (String -> String)
toHaskellCommentsDef = haskellSerdeDefinition "toHaskellComments" $
  lambda "c" $ Strings.intercalate (string "\n") $ Lists.map (lambda "s" $ Strings.cat2 (string "-- | ") (var "s")) (Strings.lines $ var "c")

writeQualifiedNameDef :: TElement (H.QualifiedName -> String)
writeQualifiedNameDef = haskellSerdeDefinition "writeQualifiedName" $
  lambda "qname" $ lets [
    "qualifiers">: project H._QualifiedName H._QualifiedName_qualifiers @@ var "qname",
    "unqual">: project H._QualifiedName H._QualifiedName_unqualified @@ var "qname",
    "h">: lambda "namePart" $ unwrap H._NamePart @@ var "namePart",
    "allParts">: Lists.concat2 (Lists.map (var "h") (var "qualifiers")) (list [var "h" @@ var "unqual"])] $
    Strings.intercalate (string ".") (var "allParts")
