
module Hydra.Sources.Haskell.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants      as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.Haskell.Syntax as H
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators


haskellSerdeDefinition :: String -> TTerm a -> TTermDefinition a
haskellSerdeDefinition = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.haskell.serde"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Serialization.ns, HaskellOperators.ns]
    (HaskellSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just ("Haskell operator precendence and associativity are drawn from:\n"
      <> "https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html\n"
      <> "Other operators were investigated using GHCi, e.g. \":info (->)\"\n"
      <> "Operator names are drawn (loosely) from:\n"
      <> "https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators")
  where
    definitions = [
      toDefinition alternativeToExpr,
      toDefinition applicationExpressionToExpr,
      toDefinition applicationPatternToExpr,
      toDefinition assertionToExpr,
      toDefinition caseExpressionToExpr,
      toDefinition caseRhsToExpr,
      toDefinition classAssertionToExpr,
      toDefinition constructorToExpr,
      toDefinition constructorWithCommentsToExpr,
      toDefinition dataOrNewtypeToExpr,
      toDefinition declarationHeadToExpr,
      toDefinition declarationToExpr,
      toDefinition declarationWithCommentsToExpr,
      toDefinition expressionToExpr,
      toDefinition constructRecordExpressionToExpr,
      toDefinition fieldToExpr,
      toDefinition fieldWithCommentsToExpr,
      toDefinition ifExpressionToExpr,
      toDefinition importExportSpecToExpr,
      toDefinition importToExpr,
      toDefinition lambdaExpressionToExpr,
      toDefinition literalToExpr,
      toDefinition localBindingToExpr,
      toDefinition moduleHeadToExpr,
      toDefinition moduleToExpr,
      toDefinition nameToExpr,
      toDefinition patternToExpr,
      toDefinition rightHandSideToExpr,
      toDefinition statementToExpr,
      toDefinition typeSignatureToExpr,
      toDefinition typeToExpr,
      toDefinition valueBindingToExpr,
      toDefinition variableToExpr,
      toDefinition toHaskellComments,
      toDefinition toSimpleComments,
      toDefinition writeQualifiedName]

alternativeToExpr :: TTermDefinition (H.Alternative -> Expr)
alternativeToExpr = haskellSerdeDefinition "alternativeToExpr" $
  doc "Convert a pattern-matching alternative to an AST expression" $
  lambda "alt" $
    -- Use structuralSpaceSep to avoid triggering parenthesize on the RHS
    Serialization.structuralSpaceSep @@ list [
      patternToExpr @@ (project H._Alternative H._Alternative_pattern @@ var "alt"),
      Serialization.cst @@ string "->",
      caseRhsToExpr @@ (project H._Alternative H._Alternative_rhs @@ var "alt")]

applicationExpressionToExpr :: TTermDefinition (H.ApplicationExpression -> Expr)
applicationExpressionToExpr = haskellSerdeDefinition "applicationExpressionToExpr" $
  doc "Convert a function application expression to an AST expression" $
  lambda "app" $
    Serialization.ifx @@ HaskellOperators.appOp @@
      (expressionToExpr @@ (project H._ApplicationExpression H._ApplicationExpression_function @@ var "app")) @@
      (expressionToExpr @@ (project H._ApplicationExpression H._ApplicationExpression_argument @@ var "app"))

applicationPatternToExpr :: TTermDefinition (H.ApplicationPattern -> Expr)
applicationPatternToExpr = haskellSerdeDefinition "applicationPatternToExpr" $
  doc "Convert an application pattern to an AST expression" $
  lambda "appPat" $ lets [
            "name">: project H._ApplicationPattern H._ApplicationPattern_name @@ var "appPat",
    "pats">: project H._ApplicationPattern H._ApplicationPattern_args @@ var "appPat"] $
    Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (Lists.map (patternToExpr) (var "pats")))

assertionToExpr :: TTermDefinition (H.Assertion -> Expr)
assertionToExpr = haskellSerdeDefinition "assertionToExpr" $
  doc "Convert a type class assertion to an AST expression" $
  lambda "sert" $
    cases H._Assertion (var "sert") Nothing [
      H._Assertion_class>>: lambda "cls" $ classAssertionToExpr @@ var "cls",
      H._Assertion_tuple>>: lambda "serts" $
        Serialization.parenList @@ false @@ (Lists.map (assertionToExpr) (var "serts"))]

caseExpressionToExpr :: TTermDefinition (H.CaseExpression -> Expr)
caseExpressionToExpr = haskellSerdeDefinition "caseExpressionToExpr" $
  doc "Convert a case expression to an AST expression" $
  lambda "caseExpr" $ lets [
    "cs">: project H._CaseExpression H._CaseExpression_case @@ var "caseExpr",
    "alts">: project H._CaseExpression H._CaseExpression_alternatives @@ var "caseExpr",
    "ofOp">: Ast.op
      (Ast.symbol $ string "of")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone,
    "lhs">: Serialization.spaceSep @@ list [Serialization.cst @@ (string "case"), expressionToExpr @@ var "cs"],
    "rhs">: Serialization.newlineSep @@ (Lists.map (alternativeToExpr) (var "alts"))] $
    Serialization.ifx @@ var "ofOp" @@ var "lhs" @@ var "rhs"

caseRhsToExpr :: TTermDefinition (H.CaseRhs -> Expr)
caseRhsToExpr = haskellSerdeDefinition "caseRhsToExpr" $
  doc "Convert a case right-hand side to an AST expression" $
  lambda "rhs" $ expressionToExpr @@ (unwrap H._CaseRhs @@ var "rhs")

classAssertionToExpr :: TTermDefinition (H.ClassAssertion -> Expr)
classAssertionToExpr = haskellSerdeDefinition "classAssertionToExpr" $
  doc "Convert a class assertion to an AST expression" $
  lambda "clsAsrt" $ lets [
    "name">: project H._ClassAssertion H._ClassAssertion_name @@ var "clsAsrt",
    "types">: project H._ClassAssertion H._ClassAssertion_types @@ var "clsAsrt"] $
    Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (list [
      Serialization.commaSep @@ Serialization.halfBlockStyle @@ (Lists.map (typeToExpr) (var "types"))]))

constructorToExpr :: TTermDefinition (H.Constructor -> Expr)
constructorToExpr = haskellSerdeDefinition "constructorToExpr" $
  doc "Convert a data constructor to an AST expression" $
  lambda "cons" $
    cases H._Constructor (var "cons") Nothing [
      H._Constructor_ordinary>>: lambda "ord" $ lets [
        "name">: project H._OrdinaryConstructor H._OrdinaryConstructor_name @@ var "ord",
        "types">: project H._OrdinaryConstructor H._OrdinaryConstructor_fields @@ var "ord"] $
        Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (list [Serialization.spaceSep @@ (Lists.map (typeToExpr) (var "types"))])),
      H._Constructor_record>>: lambda "rec" $ lets [
        "name">: project H._RecordConstructor H._RecordConstructor_name @@ var "rec",
        "fields">: project H._RecordConstructor H._RecordConstructor_fields @@ var "rec"] $
        Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (list [
          Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ (Lists.map (fieldWithCommentsToExpr) (var "fields"))]))]

constructorWithCommentsToExpr :: TTermDefinition (H.ConstructorWithComments -> Expr)
constructorWithCommentsToExpr = haskellSerdeDefinition "constructorWithCommentsToExpr" $
  doc "Convert a data constructor with comments to an AST expression" $
  lambda "consWithComments" $ lets [
    "body">: project H._ConstructorWithComments H._ConstructorWithComments_body @@ var "consWithComments",
    "mc">: project H._ConstructorWithComments H._ConstructorWithComments_comments @@ var "consWithComments"] $
    Maybes.maybe
      (constructorToExpr @@ var "body")
      (lambda "c" $ Serialization.newlineSep @@ (Lists.cons (Serialization.cst @@ (toHaskellComments @@ var "c")) (list [
        constructorToExpr @@ var "body"])))
      (var "mc")

dataOrNewtypeToExpr :: TTermDefinition (H.DataOrNewtype -> Expr)
dataOrNewtypeToExpr = haskellSerdeDefinition "dataOrNewtypeToExpr" $
  doc "Convert a data/newtype keyword to an AST expression" $
  lambda "kw" $
    cases H._DataOrNewtype (var "kw") Nothing [
      H._DataOrNewtype_data>>: constant $ Serialization.cst @@ string "data",
      H._DataOrNewtype_newtype>>: constant $ Serialization.cst @@ string "newtype"]

declarationHeadToExpr :: TTermDefinition (H.DeclarationHead -> Expr)
declarationHeadToExpr = haskellSerdeDefinition "declarationHeadToExpr" $
  doc "Convert a declaration head to an AST expression" $
  lambda "hd" $
    cases H._DeclarationHead (var "hd") Nothing [
      H._DeclarationHead_application>>: lambda "appHead" $ lets [
        "fun">: project H._ApplicationDeclarationHead H._ApplicationDeclarationHead_function @@ var "appHead",
        "op">: project H._ApplicationDeclarationHead H._ApplicationDeclarationHead_operand @@ var "appHead"] $
        Serialization.spaceSep @@ (Lists.cons (declarationHeadToExpr @@ var "fun") (list [variableToExpr @@ var "op"])),
      H._DeclarationHead_simple>>: lambda "name" $ nameToExpr @@ var "name"]

declarationToExpr :: TTermDefinition (H.Declaration -> Expr)
declarationToExpr = haskellSerdeDefinition "declarationToExpr" $
  doc "Convert a declaration to an AST expression" $
  lambda "decl" $
    cases H._Declaration (var "decl") Nothing [
      H._Declaration_data>>: lambda "dataDecl" $ lets [
        "kw">: project H._DataDeclaration H._DataDeclaration_keyword @@ var "dataDecl",
        "hd">: project H._DataDeclaration H._DataDeclaration_head @@ var "dataDecl",
        "cons">: project H._DataDeclaration H._DataDeclaration_constructors @@ var "dataDecl",
        "deriv">: project H._DataDeclaration H._DataDeclaration_deriving @@ var "dataDecl",
        "derivCat">: Lists.concat $ Lists.map (unwrap H._Deriving) (var "deriv"),
        "constructors">: Serialization.orSep @@ Serialization.halfBlockStyle @@ (Lists.map (constructorWithCommentsToExpr) (var "cons")),
        "derivingClause">: Logic.ifElse (Lists.null $ var "derivCat")
          (list ([] :: [TTerm Expr]))
          (list [Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "deriving")) (list [
            Serialization.parenList @@ false @@ (Lists.map (nameToExpr) (var "derivCat"))]))]),
        "mainParts">: list [
          Serialization.spaceSep @@ (Lists.cons (dataOrNewtypeToExpr @@ var "kw") (Lists.cons (declarationHeadToExpr @@ var "hd") (list [Serialization.cst @@ (string "=")]))),
          var "constructors"]] $
        Serialization.indentBlock @@ Lists.concat2 (var "mainParts") (var "derivingClause"),
      H._Declaration_type>>: lambda "typeDecl" $ lets [
        "hd">: project H._TypeDeclaration H._TypeDeclaration_name @@ var "typeDecl",
        "typ">: project H._TypeDeclaration H._TypeDeclaration_type @@ var "typeDecl"] $
        Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "type")) (Lists.cons (declarationHeadToExpr @@ var "hd") (Lists.cons (Serialization.cst @@ (string "=")) (list [
          typeToExpr @@ var "typ"])))),
      H._Declaration_valueBinding>>: lambda "vb" $ valueBindingToExpr @@ var "vb",
      H._Declaration_typedBinding>>: lambda "typedBinding" $ lets [
        "typeSig">: project H._TypedBinding H._TypedBinding_typeSignature @@ var "typedBinding",
        "vb">: project H._TypedBinding H._TypedBinding_valueBinding @@ var "typedBinding",
        "name">: project H._TypeSignature H._TypeSignature_name @@ var "typeSig",
        "htype">: project H._TypeSignature H._TypeSignature_type @@ var "typeSig"] $
        Serialization.newlineSep @@ (Lists.cons (Serialization.structuralSpaceSep @@ list [nameToExpr @@ var "name", Serialization.cst @@ string "::", typeToExpr @@ var "htype"]) (list [
          valueBindingToExpr @@ var "vb"]))]

declarationWithCommentsToExpr :: TTermDefinition (H.DeclarationWithComments -> Expr)
declarationWithCommentsToExpr = haskellSerdeDefinition "declarationWithCommentsToExpr" $
  doc "Convert a declaration with comments to an AST expression" $
  lambda "declWithComments" $ lets [
    "body">: project H._DeclarationWithComments H._DeclarationWithComments_body @@ var "declWithComments",
    "mc">: project H._DeclarationWithComments H._DeclarationWithComments_comments @@ var "declWithComments"] $
    Maybes.maybe
      (declarationToExpr @@ var "body")
      (lambda "c" $ Serialization.newlineSep @@ (Lists.cons (Serialization.cst @@ (toHaskellComments @@ var "c")) (list [
        declarationToExpr @@ var "body"])))
      (var "mc")

expressionToExpr :: TTermDefinition (H.Expression -> Expr)
expressionToExpr = haskellSerdeDefinition "expressionToExpr" $
  doc "Convert a Haskell expression to an AST expression" $
  lambda "expr" $
    cases H._Expression (var "expr") Nothing [
      H._Expression_application>>: lambda "app" $ applicationExpressionToExpr @@ var "app",
      H._Expression_case>>: lambda "cases" $ caseExpressionToExpr @@ var "cases",
      H._Expression_constructRecord>>: lambda "r" $ constructRecordExpressionToExpr @@ var "r",
      H._Expression_do>>: lambda "statements" $
        Serialization.indentBlock @@ Lists.cons (Serialization.cst @@ (string "do")) (Lists.map (statementToExpr) (var "statements")),
      H._Expression_if>>: lambda "ifte" $ ifExpressionToExpr @@ var "ifte",
      H._Expression_literal>>: lambda "lit" $ literalToExpr @@ var "lit",
      H._Expression_lambda>>: lambda "lam" $ Serialization.parenthesize @@ (lambdaExpressionToExpr @@ var "lam"),
      H._Expression_let>>: lambda "letExpr" $ lets [
        "bindings">: project H._LetExpression H._LetExpression_bindings @@ var "letExpr",
        "inner">: project H._LetExpression H._LetExpression_inner @@ var "letExpr",
        "encodeBinding">: lambda "binding" $
          Serialization.indentSubsequentLines @@ (string "    ") @@ (localBindingToExpr @@ var "binding")] $
        Serialization.indentBlock @@ (Lists.cons (Serialization.cst @@ (string "")) (Lists.cons
          (Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "let")) (list [Serialization.customIndentBlock @@ (string "    ") @@ (Lists.map (var "encodeBinding") (var "bindings"))])))
          (list [Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "in")) (list [expressionToExpr @@ var "inner"]))]))),
      H._Expression_list>>: lambda "exprs" $
        Serialization.bracketList @@ Serialization.halfBlockStyle @@ (Lists.map (expressionToExpr) (var "exprs")),
      H._Expression_parens>>: lambda "expr'" $ Serialization.parenthesize @@ (expressionToExpr @@ var "expr'"),
      H._Expression_tuple>>: lambda "exprs" $
        Serialization.parenList @@ false @@ (Lists.map (expressionToExpr) (var "exprs")),
      H._Expression_variable>>: lambda "name" $ nameToExpr @@ var "name"]

constructRecordExpressionToExpr :: TTermDefinition (H.ConstructRecordExpression -> Expr)
constructRecordExpressionToExpr = haskellSerdeDefinition "constructRecordExpressionToExpr" $
  doc "Convert a record construction expression to an AST expression" $
  lambda "constructRecord" $ lets [
    "name">: project H._ConstructRecordExpression H._ConstructRecordExpression_name @@ var "constructRecord",
    "updates">: project H._ConstructRecordExpression H._ConstructRecordExpression_fields @@ var "constructRecord",
    "fromUpdate">: lambda "update" $ lets [
      "fn">: project H._FieldUpdate H._FieldUpdate_name @@ var "update",
      "val">: project H._FieldUpdate H._FieldUpdate_value @@ var "update"] $
      Serialization.ifx @@ HaskellOperators.defineOp @@ (nameToExpr @@ var "fn") @@ (expressionToExpr @@ var "val"),
    "body">: Serialization.commaSep @@ Serialization.halfBlockStyle @@ (Lists.map (var "fromUpdate") (var "updates"))] $
    Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (list [
      Serialization.brackets @@ Serialization.curlyBraces @@ Serialization.halfBlockStyle @@ var "body"]))

fieldToExpr :: TTermDefinition (H.Field -> Expr)
fieldToExpr = haskellSerdeDefinition "fieldToExpr" $
  doc "Convert a field declaration to an AST expression" $
  lambda "field" $ lets [
    "name">: project H._Field H._Field_name @@ var "field",
    "typ">: project H._Field H._Field_type @@ var "field"] $
    Serialization.spaceSep @@ (Lists.cons (nameToExpr @@ var "name") (Lists.cons (Serialization.cst @@ (string "::")) (list [typeToExpr @@ var "typ"])))

fieldWithCommentsToExpr :: TTermDefinition (H.FieldWithComments -> Expr)
fieldWithCommentsToExpr = haskellSerdeDefinition "fieldWithCommentsToExpr" $
  doc "Convert a field with comments to an AST expression" $
  lambda "fieldWithComments" $ lets [
    "field">: project H._FieldWithComments H._FieldWithComments_field @@ var "fieldWithComments",
    "mc">: project H._FieldWithComments H._FieldWithComments_comments @@ var "fieldWithComments"] $
    Maybes.maybe
      (fieldToExpr @@ var "field")
      (lambda "c" $ Serialization.newlineSep @@ (Lists.cons (Serialization.cst @@ (toHaskellComments @@ var "c")) (list [
        fieldToExpr @@ var "field"])))
      (var "mc")

ifExpressionToExpr :: TTermDefinition (H.IfExpression -> Expr)
ifExpressionToExpr = haskellSerdeDefinition "ifExpressionToExpr" $
  doc "Convert an if-then-else expression to an AST expression" $
  lambda "ifExpr" $ lets [
    "eif">: project H._IfExpression H._IfExpression_condition @@ var "ifExpr",
    "ethen">: project H._IfExpression H._IfExpression_then @@ var "ifExpr",
    "eelse">: project H._IfExpression H._IfExpression_else @@ var "ifExpr",
    "ifOp">: Ast.op
      (Ast.symbol $ string "")
      (Ast.padding Ast.wsNone (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone,
    "body">: Serialization.newlineSep @@ (Lists.cons
      (Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "then")) (list [expressionToExpr @@ var "ethen"])))
      (list [Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "else")) (list [expressionToExpr @@ var "eelse"]))]))] $
    Serialization.ifx @@ var "ifOp" @@
      (Serialization.spaceSep @@ (Lists.cons (Serialization.cst @@ (string "if")) (list [expressionToExpr @@ var "eif"]))) @@
      var "body"

importExportSpecToExpr :: TTermDefinition (H.ImportExportSpec -> Expr)
importExportSpecToExpr = haskellSerdeDefinition "importExportSpecToExpr" $
  doc "Convert an import/export specification to an AST expression" $
  lambda "spec" $ nameToExpr @@ (project H._ImportExportSpec H._ImportExportSpec_name @@ var "spec")

importToExpr :: TTermDefinition (H.Import -> Expr)
importToExpr = haskellSerdeDefinition "importToExpr" $
  doc "Convert an import statement to an AST expression" $
  lambda "import" $ lets [
    "qual">: project H._Import H._Import_qualified @@ var "import",
    "modName">: project H._Import H._Import_module @@ var "import",
    "mod">: project H._Import H._Import_as @@ var "import",
    "mspec">: project H._Import H._Import_spec @@ var "import",
    "name">: unwrap H._ModuleName @@ var "modName",
    "hidingSec">: lambda "spec" $
      cases H._SpecImport (var "spec") Nothing [
        H._SpecImport_hiding>>: lambda "names" $
          Serialization.spaceSep @@ (Lists.cons
            (Serialization.cst @@ (string "hiding "))
            (list [Serialization.parens @@
              (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map (importExportSpecToExpr) (var "names")))]))],
    "parts">: Maybes.cat $ list [
      just $ Serialization.cst @@ (string "import"),
      Logic.ifElse (var "qual") (just $ Serialization.cst @@ (string "qualified")) nothing,
      just $ Serialization.cst @@ var "name",
      Maybes.map (lambda "m" $ Serialization.cst @@ (Strings.cat2 (string "as ") (unwrap H._ModuleName @@ var "m"))) (var "mod"),
      Maybes.map (var "hidingSec") (var "mspec")]] $
    Serialization.spaceSep @@ var "parts"

lambdaExpressionToExpr :: TTermDefinition (H.LambdaExpression -> Expr)
lambdaExpressionToExpr = haskellSerdeDefinition "lambdaExpressionToExpr" $
  doc "Convert a lambda expression to an AST expression" $
  lambda "lambdaExpr" $ lets [
    "bindings">: project H._LambdaExpression H._LambdaExpression_bindings @@ var "lambdaExpr",
            "inner">: project H._LambdaExpression H._LambdaExpression_inner @@ var "lambdaExpr",
    "head">: Serialization.spaceSep @@ (Lists.map (patternToExpr) (var "bindings")),
    "body">: expressionToExpr @@ var "inner"] $
    Serialization.ifx @@ HaskellOperators.lambdaOp @@
      (Serialization.prefix @@ (string "\\") @@ var "head") @@
      var "body"

--literalToExpr :: TTermDefinition (H.Literal -> Expr)
--literalToExpr = haskellSerdeDefinition "literalToExpr" $
--  "lit" ~>
--  "parensIfNeg" <~ ("b" ~> "e" ~> Logic.ifElse (var "b")
--    (Strings.cat $ list ["(", var "e", ")"])
--    (var "e")) $
--  Serialization.cst @@
--    cases H._Literal (var "lit") Nothing [
--      H._Literal_char>>: lambda "c" $ Literals.showString $ Literals.showUint16 $ var "c", -- Simplified char handling
--      H._Literal_double>>: "d" ~> var "parensIfNeg"
--        @@ (Equality.lt (var "d") (float64 0.0))
--        @@ (Literals.showFloat64 $ var "d"),
--      H._Literal_float>>: "f" ~> var "parensIfNeg"
--        @@ (Equality.lt (var "f") (float32 0.0))
--        @@ (Literals.showFloat32 $ var "f"),
--      H._Literal_int>>: "i" ~> var "parensIfNeg"
--        @@ (Equality.lt (var "i") (int32 0))
--        @@ (Literals.showInt32 $ var "i"),
--
--      H._Literal_integer>>: lambda "i" $ Literals.showBigint $ var "i",
--      H._Literal_string>>: lambda "s" $ Literals.showString $ var "s"]


-- KNOWN LIMITATION: when generating Haskell source from Double/Float values,
-- NaN and Infinity come through as "NaN"/"Infinity"/"-Infinity" (from Haskell's Show),
-- which are not valid Haskell literals. The gen-main/haskell/Hydra/Ext/Haskell/Serde.hs
-- includes a bootstrap patch that emits (0/0), (1/0), and (-1/0) in those cases.
-- If this DSL source is regenerated, the patch in the generated Serde.hs MUST be
-- restored via bin/patch-haskell-serde.sh — otherwise test-suite generation will
-- emit uncompilable Haskell for primitives that produce NaN/Inf outputs.
-- See feature_312_float_tests.
literalToExpr :: TTermDefinition (H.Literal -> Expr)
literalToExpr = haskellSerdeDefinition "literalToExpr" $
  doc "Convert a literal value to an AST expression" $
  "lit" ~>
  "parensIfNeg" <~ ("b" ~> "e" ~> Logic.ifElse (var "b")
    (Strings.cat $ list [string "(", var "e", string ")"])
    (var "e")) $
  Serialization.cst @@
    cases H._Literal (var "lit") Nothing [
      H._Literal_char>>: "c" ~> Literals.showString $ Literals.showUint16 $ var "c", -- Simplified char handling
      H._Literal_double>>: "d" ~> var "parensIfNeg"
        @@ (Equality.lt (var "d") (float64 0.0))
        @@ (Literals.showFloat64 $ var "d"),
      H._Literal_float>>: "f" ~> var "parensIfNeg"
        @@ (Equality.lt (var "f") (float32 0.0))
        @@ (Literals.showFloat32 $ var "f"),
      H._Literal_int>>: "i" ~> var "parensIfNeg"
        @@ (Equality.lt (var "i") (int32 0))
        @@ (Literals.showInt32 $ var "i"),
      H._Literal_integer>>: "i" ~> var "parensIfNeg"
        @@ (Equality.lt (var "i") (bigint 0))
        @@ (Literals.showBigint $ var "i"),
      H._Literal_string>>: lambda "s" $ Literals.showString $ var "s"]

localBindingToExpr :: TTermDefinition (H.LocalBinding -> Expr)
localBindingToExpr = haskellSerdeDefinition "localBindingToExpr" $
  doc "Convert a local binding to an AST expression" $
  lambda "binding" $
    cases H._LocalBinding (var "binding") Nothing [
      H._LocalBinding_signature>>: lambda "ts" $ typeSignatureToExpr @@ var "ts",
      H._LocalBinding_value>>: lambda "vb" $ valueBindingToExpr @@ var "vb"]

moduleHeadToExpr :: TTermDefinition (H.ModuleHead -> Expr)
moduleHeadToExpr = haskellSerdeDefinition "moduleHeadToExpr" $
  doc "Convert a module head to an AST expression" $
  lambda "moduleHead" $ lets [
    "mc">: project H._ModuleHead H._ModuleHead_comments @@ var "moduleHead",
    "modName">: project H._ModuleHead H._ModuleHead_name @@ var "moduleHead",
    "mname">: unwrap H._ModuleName @@ var "modName",
    "head">: Serialization.spaceSep @@ (Lists.cons
      (Serialization.cst @@ (string "module")) (Lists.cons
      (Serialization.cst @@ var "mname")
      (list [Serialization.cst @@ (string "where")])))] $
    Maybes.maybe
      (var "head")
      (lambda "c" $ Serialization.newlineSep @@ (Lists.cons
        (Serialization.cst @@ (toHaskellComments @@ var "c")) (Lists.cons
        (Serialization.cst @@ (string ""))
        (list [var "head"]))))
      (var "mc")

moduleToExpr :: TTermDefinition (H.Module -> Expr)
moduleToExpr = haskellSerdeDefinition "moduleToExpr" $
  doc "Convert a Haskell module to an AST expression" $
  lambda "module" $ lets [
    "mh">: project H._Module H._Module_head @@ var "module",
    "imports">: project H._Module H._Module_imports @@ var "module",
    "decls">: project H._Module H._Module_declarations @@ var "module",
    "warning">: list [Serialization.cst @@ (toSimpleComments @@ Constants.warningAutoGeneratedFile)],
    "headerLine">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "h" $ list [moduleHeadToExpr @@ var "h"]) (var "mh"),
    "declLines">: Lists.map (declarationWithCommentsToExpr) (var "decls"),
    "importLines">: Logic.ifElse (Lists.null $ var "imports")
      (list ([] :: [TTerm Expr]))
      (list [Serialization.newlineSep @@ (Lists.map (importToExpr) (var "imports"))])] $
    Serialization.doubleNewlineSep @@ (Lists.concat $ list [var "warning", var "headerLine", var "importLines", var "declLines"])

nameToExpr :: TTermDefinition (H.Name -> Expr)
nameToExpr = haskellSerdeDefinition "nameToExpr" $
  doc "Convert a Haskell name to an AST expression" $
  lambda "name" $
    Serialization.cst @@
      cases H._Name (var "name") Nothing [
        H._Name_implicit>>: lambda "qn" $ Strings.cat2 (string "?") (writeQualifiedName @@ var "qn"),
        H._Name_normal>>: lambda "qn" $ writeQualifiedName @@ var "qn",
        H._Name_parens>>: lambda "qn" $ Strings.cat $ list [string "(", writeQualifiedName @@ var "qn", string ")"]]

patternToExpr :: TTermDefinition (H.Pattern -> Expr)
patternToExpr = haskellSerdeDefinition "patternToExpr" $
  doc "Convert a pattern to an AST expression" $
  lambda "pat" $
    cases H._Pattern (var "pat") Nothing [
      H._Pattern_application>>: lambda "app" $ applicationPatternToExpr @@ var "app",
      H._Pattern_list>>: lambda "pats" $
        Serialization.bracketList @@ Serialization.halfBlockStyle @@ (Lists.map (patternToExpr) (var "pats")),
      H._Pattern_literal>>: lambda "lit" $ literalToExpr @@ var "lit",
      H._Pattern_name>>: lambda "name" $ nameToExpr @@ var "name",
      H._Pattern_parens>>: lambda "pat'" $ Serialization.parenthesize @@ (patternToExpr @@ var "pat'"),
      H._Pattern_tuple>>: lambda "pats" $
        Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "pats")),
      H._Pattern_wildcard>>: constant $ Serialization.cst @@ (string "_")]

rightHandSideToExpr :: TTermDefinition (H.RightHandSide -> Expr)
rightHandSideToExpr = haskellSerdeDefinition "rightHandSideToExpr" $
  doc "Convert a right-hand side to an AST expression" $
  lambda "rhs" $ expressionToExpr @@ (unwrap H._RightHandSide @@ var "rhs")

statementToExpr :: TTermDefinition (H.Statement -> Expr)
statementToExpr = haskellSerdeDefinition "statementToExpr" $
  doc "Convert a statement to an AST expression" $
  lambda "stmt" $ expressionToExpr @@ (unwrap H._Statement @@ var "stmt")

typeSignatureToExpr :: TTermDefinition (H.TypeSignature -> Expr)
typeSignatureToExpr = haskellSerdeDefinition "typeSignatureToExpr" $
  doc "Convert a type signature to an AST expression" $
  lambda "typeSig" $ lets [
    "name">: project H._TypeSignature H._TypeSignature_name @@ var "typeSig",
    "typ">: project H._TypeSignature H._TypeSignature_type @@ var "typeSig",
    "nameExpr">: nameToExpr @@ var "name",
    "typeExpr">: typeToExpr @@ var "typ",
    "inlineSig">: Serialization.structuralSpaceSep @@ list [var "nameExpr", Serialization.cst @@ string "::", var "typeExpr"]] $
    -- If the inline form exceeds 120 chars, break the type onto the next line with indentation
    Logic.ifElse (Equality.gt (Serialization.expressionLength @@ var "inlineSig") (int32 120))
      (Serialization.newlineSep @@ list [
        Serialization.spaceSep @@ list [var "nameExpr", Serialization.cst @@ string "::"],
        Serialization.tabIndent @@ var "typeExpr"])
      (var "inlineSig")

typeToExpr :: TTermDefinition (H.Type -> Expr)
typeToExpr = haskellSerdeDefinition "typeToExpr" $
  doc "Convert a Haskell type to an AST expression" $
  lambda "htype" $
    cases H._Type (var "htype") Nothing [
      H._Type_application>>: lambda "appType" $ lets [
        "lhs">: project H._ApplicationType H._ApplicationType_context @@ var "appType",
        "rhs">: project H._ApplicationType H._ApplicationType_argument @@ var "appType"] $
        Serialization.ifx @@ HaskellOperators.appOp @@ (typeToExpr @@ var "lhs") @@ (typeToExpr @@ var "rhs"),
      H._Type_ctx>>: lambda "ctxType" $ lets [
        "ctx">: project H._ContextType H._ContextType_ctx @@ var "ctxType",
        "typ">: project H._ContextType H._ContextType_type @@ var "ctxType"] $
        Serialization.ifx @@ HaskellOperators.assertOp @@ (assertionToExpr @@ var "ctx") @@ (typeToExpr @@ var "typ"),
      H._Type_function>>: lambda "funType" $ lets [
        "dom">: project H._FunctionType H._FunctionType_domain @@ var "funType",
        "cod">: project H._FunctionType H._FunctionType_codomain @@ var "funType"] $
        Serialization.ifx @@ HaskellOperators.arrowOp @@ (typeToExpr @@ var "dom") @@ (typeToExpr @@ var "cod"),
      H._Type_list>>: lambda "htype'" $
        Serialization.bracketList @@ Serialization.inlineStyle @@ list [typeToExpr @@ var "htype'"],
      H._Type_tuple>>: lambda "types" $
        Serialization.parenList @@ false @@ (Lists.map (typeToExpr) (var "types")),
      H._Type_variable>>: lambda "name" $ nameToExpr @@ var "name"]

valueBindingToExpr :: TTermDefinition (H.ValueBinding -> Expr)
valueBindingToExpr = haskellSerdeDefinition "valueBindingToExpr" $
  doc "Convert a value binding to an AST expression" $
  lambda "vb" $
    cases H._ValueBinding (var "vb") Nothing [
      H._ValueBinding_simple>>: lambda "simpleVB" $ lets [
        "pat">: project H._SimpleValueBinding H._SimpleValueBinding_pattern @@ var "simpleVB",
        "rhs">: project H._SimpleValueBinding H._SimpleValueBinding_rhs @@ var "simpleVB",
        "local">: project H._SimpleValueBinding H._SimpleValueBinding_localBindings @@ var "simpleVB",
        "lhsExpr">: patternToExpr @@ var "pat",
        "rhsExpr">: rightHandSideToExpr @@ var "rhs",
        -- Use structuralSpaceSep to avoid triggering parenthesize on the RHS
        "inlineBody">: Serialization.structuralSpaceSep @@ list [var "lhsExpr", Serialization.cst @@ string "=", var "rhsExpr"],
        -- If the inline form exceeds 120 chars, break the RHS onto the next line with indentation
        "body">: Logic.ifElse (Equality.gt (Serialization.expressionLength @@ var "inlineBody") (int32 120))
          (Serialization.newlineSep @@ list [
            Serialization.spaceSep @@ list [var "lhsExpr", Serialization.cst @@ string "="],
            Serialization.tabIndent @@ var "rhsExpr"])
          (var "inlineBody")] $
        Maybes.maybe
          (var "body")
          (lambda "localBindings" $ lets [
            "bindings">: unwrap H._LocalBindings @@ var "localBindings"] $
            Serialization.indentBlock @@ (Lists.cons
              (var "body")
              (list [Serialization.indentBlock @@ Lists.cons (Serialization.cst @@ (string "where")) (Lists.map (localBindingToExpr) (var "bindings"))])))
          (var "local")]

variableToExpr :: TTermDefinition (H.Variable -> Expr)
variableToExpr = haskellSerdeDefinition "variableToExpr" $
  doc "Convert a type variable to an AST expression" $
  lambda "variable" $ nameToExpr @@ (unwrap H._Variable @@ var "variable")

toHaskellComments :: TTermDefinition (String -> String)
toHaskellComments = haskellSerdeDefinition "toHaskellComments" $
  doc "Convert a string to Haddock documentation comments" $
  lambda "c" $ Strings.intercalate (string "\n") $ Lists.map (lambda "s" $ Strings.cat2 (string "-- | ") (var "s")) (Strings.lines $ var "c")

toSimpleComments :: TTermDefinition (String -> String)
toSimpleComments = haskellSerdeDefinition "toSimpleComments" $
  doc "Convert a string to simple line comments" $
  lambda "c" $ Strings.intercalate (string "\n") $ Lists.map (lambda "s" $ Strings.cat2 (string "-- ") (var "s")) (Strings.lines $ var "c")

writeQualifiedName :: TTermDefinition (H.QualifiedName -> String)
writeQualifiedName = haskellSerdeDefinition "writeQualifiedName" $
  doc "Write a qualified name as a string" $
  lambda "qname" $ lets [
    "qualifiers">: project H._QualifiedName H._QualifiedName_qualifiers @@ var "qname",
    "unqual">: project H._QualifiedName H._QualifiedName_unqualified @@ var "qname",
    "h">: lambda "namePart" $ unwrap H._NamePart @@ var "namePart",
    "allParts">: Lists.concat2 (Lists.map (var "h") (var "qualifiers")) (list [var "h" @@ var "unqual"])] $
    Strings.intercalate (string ".") $ var "allParts"
