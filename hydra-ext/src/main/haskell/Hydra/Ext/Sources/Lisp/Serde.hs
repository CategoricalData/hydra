{-# LANGUAGE FlexibleContexts #-}

-- | Lisp serializer: converts the Lisp syntax AST to concrete syntax (source code).
-- A single serde handles all four dialects (Clojure, Emacs Lisp, Common Lisp, Scheme),
-- dispatching on the Dialect enum where concrete syntax diverges.

module Hydra.Ext.Sources.Lisp.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Ext.Sources.Lisp.Syntax             as LispSyntax
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as DL
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Lisp AST
import Hydra.Ast
import qualified Hydra.Ext.Lisp.Syntax as L


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.lisp.serde"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Formatting.ns, Serialization.ns]
    (LispSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme"
  where
    definitions = [
      toDefinition andExpressionToExpr,
      toDefinition applicationToExpr,
      toDefinition caseExpressionToExpr,
      toDefinition commentToExpr,
      toDefinition condExpressionToExpr,
      toDefinition constantDefinitionToExpr,
      toDefinition defKeyword,
      toDefinition defconstKeyword,
      toDefinition defnKeyword,
      toDefinition defrecordKeyword,
      toDefinition doExpressionToExpr,
      toDefinition docstringToExpr,
      toDefinition exportDeclarationToExpr,
      toDefinition expressionToExpr,
      toDefinition falseExpr,
      toDefinition fieldAccessToExpr,
      toDefinition formatLispFloat,
      toDefinition functionDefinitionToExpr,
      toDefinition ifExpressionToExpr,
      toDefinition importDeclarationToExpr,
      toDefinition keywordToExpr,
      toDefinition lambdaKeyword,
      toDefinition lambdaToExpr,
      toDefinition letExpressionToExpr,
      toDefinition listKeyword,
      toDefinition listLiteralToExpr,
      toDefinition literalToExpr,
      toDefinition macroDefinitionToExpr,
      toDefinition mapLiteralToExpr,
      toDefinition moduleDeclarationToExpr,
      toDefinition nilExpr,
      toDefinition notExpressionToExpr,
      toDefinition orExpressionToExpr,
      toDefinition programToExpr,
      toDefinition recordTypeDefinitionToExpr,
      toDefinition sExpressionToExpr,
      toDefinition setLiteralToExpr,
      toDefinition symbolToExpr,
      toDefinition topLevelFormToExpr,
      toDefinition topLevelFormWithCommentsToExpr,
      toDefinition trueExpr,
      toDefinition variableDefinitionToExpr,
      toDefinition variableReferenceToExpr,
      toDefinition vectorLiteralToExpr]

-- | Square brackets: [expr1 expr2 ...]
sqBrackets :: TTerm [Expr] -> TTerm Expr
sqBrackets exprs = Serialization.brackets @@ (asTerm Serialization.squareBrackets) @@ (asTerm Serialization.inlineStyle) @@
  (Serialization.spaceSep @@ exprs)

-- | Parenthesized space-separated list: (expr1 expr2 ...)
parenList :: TTerm [Expr] -> TTerm Expr
parenList exprs = Serialization.parens @@ (Serialization.spaceSep @@ exprs)

-- | Serialize an and expression: (and expr1 expr2 ...)
andExpressionToExpr :: TTermDefinition (L.Dialect -> L.AndExpression -> Expr)
andExpressionToExpr = define "andExpressionToExpr" $
  lambda "d" $ lambda "andExpr" $
    Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
      (list [Serialization.cst @@ string "and"])
      (Lists.map (expressionToExpr @@ var "d") (project L._AndExpression L._AndExpression_expressions @@ var "andExpr")))

-- | Serialize a function application: (f arg1 arg2 ...)
--   For Emacs Lisp: (funcall f arg1 arg2 ...) when f is not a simple variable
applicationToExpr :: TTermDefinition (L.Dialect -> L.Application -> Expr)
applicationToExpr = define "applicationToExpr" $
  lambda "d" $ lambda "app" $ lets [
    "funExpr">: project L._Application L._Application_function @@ var "app",
    "fun">: expressionToExpr @@ var "d" @@ var "funExpr",
    "args">: Lists.map (expressionToExpr @@ var "d") (project L._Application L._Application_arguments @@ var "app"),
    "needsFuncall">: cases L._Dialect (var "d") (Just $ boolean False)
      [L._Dialect_emacsLisp>>: lambda "u" $
        -- In Emacs Lisp, funcall is needed when the function is not a simple variable
        cases L._Expression (var "funExpr") (Just $ boolean True)
          [L._Expression_variable>>: lambda "s" $ boolean False]],
    "allParts">: Logic.ifElse (var "needsFuncall")
      (Lists.concat2 (list [Serialization.cst @@ string "funcall", var "fun"]) (var "args"))
      (Lists.concat2 (list [var "fun"]) (var "args"))] $
    Serialization.parens @@ (Serialization.spaceSep @@ var "allParts")

-- | Serialize a case expression
caseExpressionToExpr :: TTermDefinition (L.Dialect -> L.CaseExpression -> Expr)
caseExpressionToExpr = define "caseExpressionToExpr" $
  lambda "d" $ lambda "caseExpr" $ lets [
    "scrutinee">: expressionToExpr @@ var "d" @@ (project L._CaseExpression L._CaseExpression_scrutinee @@ var "caseExpr"),
    "clauses">: project L._CaseExpression L._CaseExpression_clauses @@ var "caseExpr",
    "dflt">: project L._CaseExpression L._CaseExpression_default @@ var "caseExpr",
    "clauseExprs">: Lists.map (lambda "c" $
      Serialization.parens @@ (Serialization.spaceSep @@ list [
        Serialization.parens @@ (Serialization.spaceSep @@
          Lists.map (expressionToExpr @@ var "d") (project L._CaseClause L._CaseClause_keys @@ var "c")),
        expressionToExpr @@ var "d" @@ (project L._CaseClause L._CaseClause_body @@ var "c")]))
      (var "clauses"),
    "defaultPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
      (lambda "e" $ list [Serialization.parens @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "else",
        expressionToExpr @@ var "d" @@ var "e"])])
      (var "dflt")] $
    Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
      list [Serialization.cst @@ string "case", var "scrutinee"],
      var "clauseExprs",
      var "defaultPart"])))

-- | Serialize a comment
commentToExpr :: TTermDefinition (L.Comment -> Expr)
commentToExpr = define "commentToExpr" $
  lambda "c" $ lets [
    "text">: project L._Comment L._Comment_text @@ var "c"] $
    Serialization.cst @@ Strings.cat2 (string "; ") (var "text")

-- | Serialize a cond expression
condExpressionToExpr :: TTermDefinition (L.Dialect -> L.CondExpression -> Expr)
condExpressionToExpr = define "condExpressionToExpr" $
  lambda "d" $ lambda "condExpr" $ lets [
    "clauses">: project L._CondExpression L._CondExpression_clauses @@ var "condExpr",
    "dflt">: project L._CondExpression L._CondExpression_default @@ var "condExpr"] $
    cases L._Dialect (var "d") Nothing [
      -- Clojure: (cond test1 expr1 test2 expr2 :else default)
      L._Dialect_clojure>>: constant $ lets [
        "clauseExprs">: Lists.concat (Lists.map (lambda "c" $ list [
          expressionToExpr @@ var "d" @@ (project L._CondClause L._CondClause_condition @@ var "c"),
          expressionToExpr @@ var "d" @@ (project L._CondClause L._CondClause_body @@ var "c")])
          (var "clauses")),
        "defaultPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
          (lambda "e" $ list [Serialization.cst @@ string ":else", expressionToExpr @@ var "d" @@ var "e"])
          (var "dflt")] $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "cond"],
          var "clauseExprs",
          var "defaultPart"]))),
      -- Others: (cond (test1 expr1) (test2 expr2) (t/else default))
      L._Dialect_emacsLisp>>: constant $ condOther (var "d") (var "clauses") (var "dflt") (string "t"),
      L._Dialect_commonLisp>>: constant $ condOther (var "d") (var "clauses") (var "dflt") (string "t"),
      L._Dialect_scheme>>: constant $ condOther (var "d") (var "clauses") (var "dflt") (string "else")]
  where
    condOther :: TTerm L.Dialect -> TTerm [L.CondClause] -> TTerm (Maybe L.Expression) -> TTerm String -> TTerm Expr
    condOther d clauses dflt defaultKeyword = lets [
      "clauseExprs">: Lists.map (lambda "c" $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          expressionToExpr @@ d @@ (project L._CondClause L._CondClause_condition @@ var "c"),
          expressionToExpr @@ d @@ (project L._CondClause L._CondClause_body @@ var "c")]))
        clauses,
      "defaultPart">: Maybes.maybe (list ([] :: [TTerm Expr]))
        (lambda "e" $ list [Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ defaultKeyword,
          expressionToExpr @@ d @@ var "e"])])
        dflt] $
      Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
        list [Serialization.cst @@ string "cond"],
        var "clauseExprs",
        var "defaultPart"])))

-- | Serialize a constant definition
constantDefinitionToExpr :: TTermDefinition (L.Dialect -> L.ConstantDefinition -> Expr)
constantDefinitionToExpr = define "constantDefinitionToExpr" $
  lambda "d" $ lambda "cdef" $ lets [
    "name">: symbolToExpr @@ (project L._ConstantDefinition L._ConstantDefinition_name @@ var "cdef"),
    "value">: expressionToExpr @@ var "d" @@ (project L._ConstantDefinition L._ConstantDefinition_value @@ var "cdef")] $
    Serialization.parens @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ (defconstKeyword @@ var "d"),
      var "name",
      var "value"])

-- | The keyword for variable definitions
defKeyword :: TTermDefinition (L.Dialect -> String)
defKeyword = define "defKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "def",
    L._Dialect_emacsLisp>>: constant $ string "defvar",
    L._Dialect_commonLisp>>: constant $ string "cl:defvar",
    L._Dialect_scheme>>: constant $ string "define"]

-- | The keyword for constant definitions
defconstKeyword :: TTermDefinition (L.Dialect -> String)
defconstKeyword = define "defconstKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "def",
    L._Dialect_emacsLisp>>: constant $ string "defconst",
    L._Dialect_commonLisp>>: constant $ string "cl:defconstant",
    L._Dialect_scheme>>: constant $ string "define"]

-- | The keyword for named function definitions
defnKeyword :: TTermDefinition (L.Dialect -> String)
defnKeyword = define "defnKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "defn",
    L._Dialect_emacsLisp>>: constant $ string "defun",
    L._Dialect_commonLisp>>: constant $ string "cl:defun",
    L._Dialect_scheme>>: constant $ string "define"]

-- | The keyword for record/struct definitions
defrecordKeyword :: TTermDefinition (L.Dialect -> String)
defrecordKeyword = define "defrecordKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "defrecord",
    L._Dialect_emacsLisp>>: constant $ string "cl-defstruct",
    L._Dialect_commonLisp>>: constant $ string "cl:defstruct",
    L._Dialect_scheme>>: constant $ string "define-record-type"]

-- | Serialize a do/progn/begin expression
doExpressionToExpr :: TTermDefinition (L.Dialect -> L.DoExpression -> Expr)
doExpressionToExpr = define "doExpressionToExpr" $
  lambda "d" $ lambda "doExpr" $ lets [
    "kw">: cases L._Dialect (var "d") Nothing [
      L._Dialect_clojure>>: constant $ string "do",
      L._Dialect_emacsLisp>>: constant $ string "progn",
      L._Dialect_commonLisp>>: constant $ string "progn",
      L._Dialect_scheme>>: constant $ string "begin"]] $
    Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
      (list [Serialization.cst @@ var "kw"])
      (Lists.map (expressionToExpr @@ var "d") (project L._DoExpression L._DoExpression_expressions @@ var "doExpr")))

-- | Serialize a docstring as a comment
docstringToExpr :: TTermDefinition (L.Docstring -> Expr)
docstringToExpr = define "docstringToExpr" $
  lambda "ds" $
    Serialization.cst @@ Strings.cat (list [string ";; ", unwrap L._Docstring @@ var "ds"])

-- | Serialize an export declaration
exportDeclarationToExpr :: TTermDefinition (L.Dialect -> L.ExportDeclaration -> Expr)
exportDeclarationToExpr = define "exportDeclarationToExpr" $
  lambda "d" $ lambda "edecl" $ lets [
    "syms">: Lists.map symbolToExpr (project L._ExportDeclaration L._ExportDeclaration_symbols @@ var "edecl")] $
    cases L._Dialect (var "d") Nothing [
      -- Clojure: public by default, no export form
      L._Dialect_clojure>>: constant $ Serialization.cst @@ string "",
      -- (provide 'name)
      L._Dialect_emacsLisp>>: constant $
        Serialization.newlineSep @@ Lists.map (lambda "s" $
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "provide",
            Serialization.noSep @@ list [Serialization.cst @@ string "'", var "s"]]))
          (var "syms"),
      -- (:export :sym1 :sym2)
      L._Dialect_commonLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
          (list [Serialization.cst @@ string ":export"])
          (Lists.map (lambda "s" $ Serialization.noSep @@ list [Serialization.cst @@ string ":", var "s"]) (var "syms"))),
      -- (export sym1 sym2)
      L._Dialect_scheme>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
          (list [Serialization.cst @@ string "export"])
          (var "syms"))]

-- | Serialize a Lisp expression
expressionToExpr :: TTermDefinition (L.Dialect -> L.Expression -> Expr)
expressionToExpr = define "expressionToExpr" $
  lambda "d" $ lambda "expr" $
    cases L._Expression (var "expr") Nothing [
      L._Expression_application>>: lambda "a" $ applicationToExpr @@ var "d" @@ var "a",
      L._Expression_lambda>>: lambda "l" $ lambdaToExpr @@ var "d" @@ var "l",
      L._Expression_let>>: lambda "l" $ letExpressionToExpr @@ var "d" @@ var "l",
      L._Expression_if>>: lambda "i" $ ifExpressionToExpr @@ var "d" @@ var "i",
      L._Expression_cond>>: lambda "c" $ condExpressionToExpr @@ var "d" @@ var "c",
      L._Expression_case>>: lambda "c" $ caseExpressionToExpr @@ var "d" @@ var "c",
      L._Expression_and>>: lambda "a" $ andExpressionToExpr @@ var "d" @@ var "a",
      L._Expression_or>>: lambda "o" $ orExpressionToExpr @@ var "d" @@ var "o",
      L._Expression_not>>: lambda "n" $ notExpressionToExpr @@ var "d" @@ var "n",
      L._Expression_do>>: lambda "e" $ doExpressionToExpr @@ var "d" @@ var "e",
      L._Expression_begin>>: lambda "e" $
        Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
          (list [Serialization.cst @@ string "begin"])
          (Lists.map (expressionToExpr @@ var "d") (project L._BeginExpression L._BeginExpression_expressions @@ var "e"))),
      L._Expression_variable>>: lambda "v" $ variableReferenceToExpr @@ var "d" @@ var "v",
      L._Expression_literal>>: lambda "l" $ literalToExpr @@ var "d" @@ var "l",
      L._Expression_list>>: lambda "l" $ listLiteralToExpr @@ var "d" @@ var "l",
      L._Expression_vector>>: lambda "v" $ vectorLiteralToExpr @@ var "d" @@ var "v",
      L._Expression_map>>: lambda "m" $ mapLiteralToExpr @@ var "d" @@ var "m",
      L._Expression_set>>: lambda "s" $ setLiteralToExpr @@ var "d" @@ var "s",
      L._Expression_cons>>: lambda "c" $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "cons",
          expressionToExpr @@ var "d" @@ (project L._ConsExpression L._ConsExpression_head @@ var "c"),
          expressionToExpr @@ var "d" @@ (project L._ConsExpression L._ConsExpression_tail @@ var "c")]),
      L._Expression_dottedPair>>: lambda "p" $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          expressionToExpr @@ var "d" @@ (project L._DottedPair L._DottedPair_car @@ var "p"),
          Serialization.cst @@ string ".",
          expressionToExpr @@ var "d" @@ (project L._DottedPair L._DottedPair_cdr @@ var "p")]),
      L._Expression_fieldAccess>>: lambda "fa" $ fieldAccessToExpr @@ var "d" @@ var "fa",
      L._Expression_typeAnnotation>>: lambda "ta" $
        expressionToExpr @@ var "d" @@ (project L._TypeAnnotation L._TypeAnnotation_expression @@ var "ta"),
      L._Expression_quote>>: lambda "q" $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "'",
          expressionToExpr @@ var "d" @@ (project L._QuoteExpression L._QuoteExpression_body @@ var "q")],
      L._Expression_quasiquote>>: lambda "q" $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "`",
          expressionToExpr @@ var "d" @@ (project L._QuasiquoteExpression L._QuasiquoteExpression_body @@ var "q")],
      L._Expression_unquote>>: lambda "u" $
        unquoteToExpr (var "d") (var "u"),
      L._Expression_splicingUnquote>>: lambda "su" $
        splicingUnquoteToExpr (var "d") (var "su"),
      L._Expression_sExpression>>: lambda "s" $ sExpressionToExpr @@ var "s"]
  where
    unquoteToExpr :: TTerm L.Dialect -> TTerm a -> TTerm Expr
    unquoteToExpr d u =
      cases L._Dialect d Nothing [
        L._Dialect_clojure>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string "~",
            expressionToExpr @@ d @@ (project L._UnquoteExpression L._UnquoteExpression_body @@ u)],
        L._Dialect_emacsLisp>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",",
            expressionToExpr @@ d @@ (project L._UnquoteExpression L._UnquoteExpression_body @@ u)],
        L._Dialect_commonLisp>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",",
            expressionToExpr @@ d @@ (project L._UnquoteExpression L._UnquoteExpression_body @@ u)],
        L._Dialect_scheme>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",",
            expressionToExpr @@ d @@ (project L._UnquoteExpression L._UnquoteExpression_body @@ u)]]
    splicingUnquoteToExpr :: TTerm L.Dialect -> TTerm a -> TTerm Expr
    splicingUnquoteToExpr d su =
      cases L._Dialect d Nothing [
        L._Dialect_clojure>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string "~@",
            expressionToExpr @@ d @@ (project L._SplicingUnquoteExpression L._SplicingUnquoteExpression_body @@ su)],
        L._Dialect_emacsLisp>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",@",
            expressionToExpr @@ d @@ (project L._SplicingUnquoteExpression L._SplicingUnquoteExpression_body @@ su)],
        L._Dialect_commonLisp>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",@",
            expressionToExpr @@ d @@ (project L._SplicingUnquoteExpression L._SplicingUnquoteExpression_body @@ su)],
        L._Dialect_scheme>>: constant $
          Serialization.noSep @@ list [
            Serialization.cst @@ string ",@",
            expressionToExpr @@ d @@ (project L._SplicingUnquoteExpression L._SplicingUnquoteExpression_body @@ su)]]

-- | Boolean false expression
falseExpr :: TTermDefinition (L.Dialect -> Expr)
falseExpr = define "falseExpr" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ Serialization.cst @@ string "false",
    L._Dialect_emacsLisp>>: constant $ Serialization.cst @@ string "nil",
    L._Dialect_commonLisp>>: constant $ Serialization.cst @@ string "cl:nil",
    L._Dialect_scheme>>: constant $ Serialization.cst @@ string "#f"]

-- | Serialize a field access expression
fieldAccessToExpr :: TTermDefinition (L.Dialect -> L.FieldAccess -> Expr)
fieldAccessToExpr = define "fieldAccessToExpr" $
  lambda "d" $ lambda "fa" $ lets [
    "rtype">: symbolToExpr @@ (project L._FieldAccess L._FieldAccess_recordType @@ var "fa"),
    "field">: symbolToExpr @@ (project L._FieldAccess L._FieldAccess_field @@ var "fa"),
    "target">: expressionToExpr @@ var "d" @@ (project L._FieldAccess L._FieldAccess_target @@ var "fa")] $
    cases L._Dialect (var "d") Nothing [
      -- (:field target) in Clojure
      L._Dialect_clojure>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.noSep @@ list [Serialization.cst @@ string ":", var "field"],
          var "target"]),
      -- (record-type-field target) in others
      L._Dialect_emacsLisp>>: constant $ fieldAccessOther (var "rtype") (var "field") (var "target"),
      L._Dialect_commonLisp>>: constant $ fieldAccessOther (var "rtype") (var "field") (var "target"),
      L._Dialect_scheme>>: constant $ fieldAccessOther (var "rtype") (var "field") (var "target")]
  where
    fieldAccessOther :: TTerm Expr -> TTerm Expr -> TTerm Expr -> TTerm Expr
    fieldAccessOther rtype field target =
      Serialization.parens @@ (Serialization.spaceSep @@ list [
        Serialization.noSep @@ list [rtype, Serialization.cst @@ string "-", field],
        target])

-- | Serialize a function definition.
-- Clojure: (defn name [params] body)
-- Elisp/CL: (defun name (params) body)
-- Scheme: (define (name params) body)
functionDefinitionToExpr :: TTermDefinition (L.Dialect -> L.FunctionDefinition -> Expr)
functionDefinitionToExpr = define "functionDefinitionToExpr" $
  lambda "d" $ lambda "fdef" $ lets [
    "name">: symbolToExpr @@ (project L._FunctionDefinition L._FunctionDefinition_name @@ var "fdef"),
    "params">: Lists.map symbolToExpr (project L._FunctionDefinition L._FunctionDefinition_params @@ var "fdef"),
    "body">: Lists.map (expressionToExpr @@ var "d") (project L._FunctionDefinition L._FunctionDefinition_body @@ var "fdef")] $
    cases L._Dialect (var "d") Nothing [
      -- (defn name [params] body...)
      L._Dialect_clojure>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defn", var "name"],
          list [sqBrackets (var "params")],
          var "body"]))),
      -- (defun name (params) body...)
      L._Dialect_emacsLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defun", var "name"],
          list [Serialization.parens @@ (Serialization.spaceSep @@ var "params")],
          var "body"]))),
      -- (defun name (params) body...)
      L._Dialect_commonLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defun", var "name"],
          list [Serialization.parens @@ (Serialization.spaceSep @@ var "params")],
          var "body"]))),
      -- (define (name params...) body...)
      L._Dialect_scheme>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "define"],
          list [Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2 (list [var "name"]) (var "params"))],
          var "body"])))]

-- | Serialize an if expression: (if test then else)
ifExpressionToExpr :: TTermDefinition (L.Dialect -> L.IfExpression -> Expr)
ifExpressionToExpr = define "ifExpressionToExpr" $
  lambda "d" $ lambda "ifExpr" $ lets [
    "cond">: expressionToExpr @@ var "d" @@ (project L._IfExpression L._IfExpression_condition @@ var "ifExpr"),
    "then">: expressionToExpr @@ var "d" @@ (project L._IfExpression L._IfExpression_then @@ var "ifExpr"),
    "else">: project L._IfExpression L._IfExpression_else @@ var "ifExpr",
    "elsePart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "e" $ list [expressionToExpr @@ var "d" @@ var "e"])
      (var "else")] $
    Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
      list [Serialization.cst @@ string "if", var "cond", var "then"],
      var "elsePart"]))

-- | Serialize an import declaration
importDeclarationToExpr :: TTermDefinition (L.Dialect -> L.ImportDeclaration -> Expr)
importDeclarationToExpr = define "importDeclarationToExpr" $
  lambda "d" $ lambda "idecl" $ lets [
    "modName">: unwrap L._NamespaceName @@ (project L._ImportDeclaration L._ImportDeclaration_module @@ var "idecl")] $
    cases L._Dialect (var "d") Nothing [
      -- (:require [name])
      L._Dialect_clojure>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string ":require",
          sqBrackets (list [Serialization.cst @@ var "modName"])]),
      -- (require 'name)
      L._Dialect_emacsLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "require",
          Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ var "modName"]]),
      -- (:use :name)
      L._Dialect_commonLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string ":use",
          Serialization.cst @@ Strings.cat2 (string ":") (var "modName")]),
      -- (import (name))
      L._Dialect_scheme>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "import",
          Serialization.parens @@ (Serialization.cst @@ var "modName")])]

-- | Serialize a keyword: :name (or 'name in Scheme)
keywordToExpr :: TTermDefinition (L.Dialect -> L.Keyword -> Expr)
keywordToExpr = define "keywordToExpr" $
  lambda "d" $ lambda "k" $ lets [
    "name">: project L._Keyword L._Keyword_name @@ var "k",
    "ns">: project L._Keyword L._Keyword_namespace @@ var "k"] $
    cases L._Dialect (var "d") (Just $
      -- Default: :name or ns/:name
      Serialization.cst @@ Maybes.maybe
        (Strings.cat2 (string ":") (var "name"))
        (lambda "n" $ Strings.cat (list [var "n", string "/:", var "name"]))
        (var "ns")) [
      -- Scheme: 'name (quoted symbol, since Scheme has no keywords)
      L._Dialect_scheme>>: constant $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "'",
          Serialization.cst @@ var "name"]]

-- | The keyword for anonymous functions
lambdaKeyword :: TTermDefinition (L.Dialect -> String)
lambdaKeyword = define "lambdaKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "fn",
    L._Dialect_emacsLisp>>: constant $ string "lambda",
    L._Dialect_commonLisp>>: constant $ string "cl:lambda",
    L._Dialect_scheme>>: constant $ string "lambda"]

-- | Serialize a lambda expression
lambdaToExpr :: TTermDefinition (L.Dialect -> L.Lambda -> Expr)
lambdaToExpr = define "lambdaToExpr" $
  lambda "d" $ lambda "lam" $ lets [
    "params">: Lists.map symbolToExpr (project L._Lambda L._Lambda_params @@ var "lam"),
    "body">: Lists.map (expressionToExpr @@ var "d") (project L._Lambda L._Lambda_body @@ var "lam"),
    "mname">: project L._Lambda L._Lambda_name @@ var "lam",
    "kw">: lambdaKeyword @@ var "d"] $
    cases L._Dialect (var "d") Nothing [
      -- (fn [params] body...) or (fn name [params] body...) if named
      L._Dialect_clojure>>: constant $
        Maybes.maybe
          -- Unnamed: (fn [params] body...)
          (Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
            list [Serialization.cst @@ var "kw"],
            list [sqBrackets (var "params")],
            var "body"]))))
          -- Named: (fn name [params] body...)
          (lambda "sym" $
            Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
              list [Serialization.cst @@ var "kw", symbolToExpr @@ var "sym"],
              list [sqBrackets (var "params")],
              var "body"]))))
          (var "mname"),
      -- (lambda (params) body...)
      L._Dialect_emacsLisp>>: constant $ lambdaOther (var "kw") (var "params") (var "body"),
      L._Dialect_commonLisp>>: constant $ lambdaOther (var "kw") (var "params") (var "body"),
      L._Dialect_scheme>>: constant $ lambdaOther (var "kw") (var "params") (var "body")]
  where
    lambdaOther :: TTerm String -> TTerm [Expr] -> TTerm [Expr] -> TTerm Expr
    lambdaOther kw params body =
      Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
        list [Serialization.cst @@ kw],
        list [Serialization.parens @@ (Serialization.spaceSep @@ params)],
        body])))

-- | Serialize a let expression
-- Scheme/CL/Elisp: (let ((name val) ...) body...) or (let*/letrec ...)
-- Clojure: (let [name val ...] body...)
letExpressionToExpr :: TTermDefinition (L.Dialect -> L.LetExpression -> Expr)
letExpressionToExpr = define "letExpressionToExpr" $
  lambda "d" $ lambda "letExpr" $ lets [
    "kind">: project L._LetExpression L._LetExpression_kind @@ var "letExpr",
    "bindings">: project L._LetExpression L._LetExpression_bindings @@ var "letExpr",
    "body">: Lists.map (expressionToExpr @@ var "d") (project L._LetExpression L._LetExpression_body @@ var "letExpr"),
    -- Extract name and value from each binding (all are simple bindings)
    "bindingPairs">: Lists.map (lambda "b" $
      cases L._LetBinding (var "b") Nothing [
        L._LetBinding_simple>>: lambda "sb" $ pair
          (symbolToExpr @@ (project L._SimpleBinding L._SimpleBinding_name @@ var "sb"))
          (expressionToExpr @@ var "d" @@ (project L._SimpleBinding L._SimpleBinding_value @@ var "sb")),
        L._LetBinding_destructuring>>: constant $ pair
          (Serialization.cst @@ string "<destructuring>")
          (Serialization.cst @@ string "<destructuring>")])
      (var "bindings")] $
    cases L._Dialect (var "d") Nothing [
      -- Clojure: (let [name val ...] body...) or (letfn [(name [params] body)] body...) for recursive
      L._Dialect_clojure>>: constant $
        cases L._LetKind (var "kind") Nothing [
          -- Recursive: split into lambda bindings (letfn) and value bindings (let).
          -- Lambda bindings → (letfn [(name [params] body) ...] ...)
          -- Value bindings → (let [name val ...] ...)
          -- Recursive: use regular let (coder reorders bindings for Clojure).
          -- Named fn handles self-reference in function bindings.
          L._LetKind_recursive>>: constant $
            clojureLet (var "bindingPairs") (var "body"),
          -- Non-recursive: (let [name val ...] body...)
          L._LetKind_parallel>>: constant $ clojureLet (var "bindingPairs") (var "body"),
          L._LetKind_sequential>>: constant $ clojureLet (var "bindingPairs") (var "body")],
      -- Others: (let/let*/letrec ((name val) ...) body...)
      L._Dialect_emacsLisp>>: constant $ letOther (var "kind") (var "bindingPairs") (var "body"),
      -- Common Lisp: emit letrec (the loader transforms it to labels or mutable cells)
      L._Dialect_commonLisp>>: constant $ letOther (var "kind") (var "bindingPairs") (var "body"),
      L._Dialect_scheme>>: constant $ letOther (var "kind") (var "bindingPairs") (var "body")]
  where
    clojureLet :: TTerm [(Expr, Expr)] -> TTerm [Expr] -> TTerm Expr
    clojureLet bindingPairs body =
      Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
        list [Serialization.cst @@ string "let"],
        list [sqBrackets (Lists.concat (Lists.map (lambda "p" $
          list [Pairs.first (var "p"), Pairs.second (var "p")]) bindingPairs))],
        body]))
    letOther :: TTerm L.LetKind -> TTerm [(Expr, Expr)] -> TTerm [Expr] -> TTerm Expr
    letOther kind bindingPairs body = lets [
      "kw">: cases L._LetKind kind Nothing [
        L._LetKind_parallel>>: constant $ string "let",
        L._LetKind_sequential>>: constant $ string "let*",
        L._LetKind_recursive>>: constant $ string "letrec"],
      "bindingExprs">: Lists.map (lambda "p" $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Pairs.first (var "p"), Pairs.second (var "p")]))
        bindingPairs] $
      Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
        list [Serialization.cst @@ var "kw"],
        list [Serialization.parens @@ (Serialization.spaceSep @@ var "bindingExprs")],
        body]))

-- | The keyword for list construction
listKeyword :: TTermDefinition (L.Dialect -> String)
listKeyword = define "listKeyword" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ string "list",
    L._Dialect_emacsLisp>>: constant $ string "list",
    L._Dialect_commonLisp>>: constant $ string "cl:list",
    L._Dialect_scheme>>: constant $ string "list"]

-- | Serialize a list literal
listLiteralToExpr :: TTermDefinition (L.Dialect -> L.ListLiteral -> Expr)
listLiteralToExpr = define "listLiteralToExpr" $
  lambda "d" $ lambda "ll" $ lets [
    "elems">: Lists.map (expressionToExpr @@ var "d") (project L._ListLiteral L._ListLiteral_elements @@ var "ll"),
    "quoted">: project L._ListLiteral L._ListLiteral_quoted @@ var "ll"] $
    Logic.ifElse (var "quoted")
      -- '(a b c)
      (Serialization.noSep @@ list [
        Serialization.cst @@ string "'",
        Serialization.parens @@ (Serialization.spaceSep @@ var "elems")])
      -- (list a b c) or (cl:list a b c)
      (Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
        (list [Serialization.cst @@ (listKeyword @@ var "d")])
        (var "elems")))

-- | Format a bigfloat value as a dialect-specific literal string.
-- Special values (NaN, ±Infinity) use dialect-specific syntax.
formatLispFloat :: TTermDefinition (L.Dialect -> Double -> String)
formatLispFloat = define "formatLispFloat" $
  lambda "d" $ lambda "v" $
    "s" <~ Literals.showBigfloat (var "v") $
    Logic.ifElse (Equality.equal (var "s") (string "NaN"))
      (cases L._Dialect (var "d") Nothing [
        L._Dialect_clojure>>: constant $ string "Double/NaN",
        L._Dialect_scheme>>: constant $ string "+nan.0",
        L._Dialect_commonLisp>>: constant $ string "+hydra-nan+",
        L._Dialect_emacsLisp>>: constant $ string "0.0e+NaN"]) $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity"))
      (cases L._Dialect (var "d") Nothing [
        L._Dialect_clojure>>: constant $ string "Double/POSITIVE_INFINITY",
        L._Dialect_scheme>>: constant $ string "+inf.0",
        L._Dialect_commonLisp>>: constant $ string "+hydra-pos-inf+",
        L._Dialect_emacsLisp>>: constant $ string "1.0e+INF"]) $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity"))
      (cases L._Dialect (var "d") Nothing [
        L._Dialect_clojure>>: constant $ string "Double/NEGATIVE_INFINITY",
        L._Dialect_scheme>>: constant $ string "-inf.0",
        L._Dialect_commonLisp>>: constant $ string "+hydra-neg-inf+",
        L._Dialect_emacsLisp>>: constant $ string "-1.0e+INF"])
      (var "s")

-- | Serialize a literal value
literalToExpr :: TTermDefinition (L.Dialect -> L.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  lambda "d" $ lambda "lit" $
    cases L._Literal (var "lit") Nothing [
      L._Literal_integer>>: lambda "i" $
        Serialization.cst @@ (Literals.showBigint (project L._IntegerLiteral L._IntegerLiteral_value @@ var "i")),
      L._Literal_float>>: lambda "f" $
        Serialization.cst @@ (formatLispFloat @@ var "d" @@ (project L._FloatLiteral L._FloatLiteral_value @@ var "f")),
      L._Literal_string>>: lambda "s" $
        -- Escape backslashes first, then control characters and double-quotes.
        -- Common Lisp does not support \n, \t, \r escape sequences in strings,
        -- so for CL we embed the literal characters directly (multi-line strings are valid).
        "e1" <~ Strings.intercalate (string "\\\\") (Strings.splitOn (string "\\") (var "s")) $
        cases L._Dialect (var "d") Nothing [
          L._Dialect_commonLisp>>: constant $
            -- CL only: escape double-quotes; leave control characters as literal bytes
            "escaped" <~ Strings.intercalate (string "\\\"") (Strings.splitOn (string "\"") (var "e1")) $
            Serialization.cst @@ (Strings.cat (list [string "\"", var "escaped", string "\""])),
          L._Dialect_clojure>>: constant $
            "e2" <~ Strings.intercalate (string "\\n") (Strings.splitOn (Strings.fromList (list [int32 10])) (var "e1")) $
            "e3" <~ Strings.intercalate (string "\\r") (Strings.splitOn (Strings.fromList (list [int32 13])) (var "e2")) $
            "e4" <~ Strings.intercalate (string "\\t") (Strings.splitOn (Strings.fromList (list [int32 9])) (var "e3")) $
            "escaped" <~ Strings.intercalate (string "\\\"") (Strings.splitOn (string "\"") (var "e4")) $
            Serialization.cst @@ (Strings.cat (list [string "\"", var "escaped", string "\""])),
          L._Dialect_emacsLisp>>: constant $
            "e2" <~ Strings.intercalate (string "\\n") (Strings.splitOn (Strings.fromList (list [int32 10])) (var "e1")) $
            "e3" <~ Strings.intercalate (string "\\r") (Strings.splitOn (Strings.fromList (list [int32 13])) (var "e2")) $
            "e4" <~ Strings.intercalate (string "\\t") (Strings.splitOn (Strings.fromList (list [int32 9])) (var "e3")) $
            "escaped" <~ Strings.intercalate (string "\\\"") (Strings.splitOn (string "\"") (var "e4")) $
            Serialization.cst @@ (Strings.cat (list [string "\"", var "escaped", string "\""])),
          L._Dialect_scheme>>: constant $
            "e2" <~ Strings.intercalate (string "\\n") (Strings.splitOn (Strings.fromList (list [int32 10])) (var "e1")) $
            "e3" <~ Strings.intercalate (string "\\r") (Strings.splitOn (Strings.fromList (list [int32 13])) (var "e2")) $
            "e4" <~ Strings.intercalate (string "\\t") (Strings.splitOn (Strings.fromList (list [int32 9])) (var "e3")) $
            "escaped" <~ Strings.intercalate (string "\\\"") (Strings.splitOn (string "\"") (var "e4")) $
            Serialization.cst @@ (Strings.cat (list [string "\"", var "escaped", string "\""]))],
      L._Literal_character>>: lambda "c" $ lets [
        "ch">: project L._CharacterLiteral L._CharacterLiteral_value @@ var "c"] $
        cases L._Dialect (var "d") Nothing [
          L._Dialect_clojure>>: constant $ Serialization.cst @@ Strings.cat2 (string "\\") (var "ch"),
          L._Dialect_emacsLisp>>: constant $ Serialization.cst @@ Strings.cat2 (string "?") (var "ch"),
          L._Dialect_commonLisp>>: constant $ Serialization.cst @@ Strings.cat2 (string "#\\") (var "ch"),
          L._Dialect_scheme>>: constant $ Serialization.cst @@ Strings.cat2 (string "#\\") (var "ch")],
      L._Literal_boolean>>: lambda "b" $
        Logic.ifElse (var "b") (trueExpr @@ var "d") (falseExpr @@ var "d"),
      L._Literal_nil>>: constant $ nilExpr @@ var "d",
      L._Literal_keyword>>: lambda "k" $ keywordToExpr @@ var "d" @@ var "k",
      L._Literal_symbol>>: lambda "s" $
        Serialization.noSep @@ list [Serialization.cst @@ string "'", symbolToExpr @@ var "s"]]

-- | Serialize a macro definition
macroDefinitionToExpr :: TTermDefinition (L.Dialect -> L.MacroDefinition -> Expr)
macroDefinitionToExpr = define "macroDefinitionToExpr" $
  lambda "d" $ lambda "mdef" $ lets [
    "name">: symbolToExpr @@ (project L._MacroDefinition L._MacroDefinition_name @@ var "mdef"),
    "params">: Lists.map symbolToExpr (project L._MacroDefinition L._MacroDefinition_params @@ var "mdef"),
    "body">: Lists.map (expressionToExpr @@ var "d") (project L._MacroDefinition L._MacroDefinition_body @@ var "mdef")] $
    cases L._Dialect (var "d") Nothing [
      -- (defmacro name [params] body)
      L._Dialect_clojure>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defmacro", var "name"],
          list [sqBrackets (var "params")],
          var "body"]))),
      -- (defmacro name (params) body)
      L._Dialect_emacsLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defmacro", var "name"],
          list [Serialization.parens @@ (Serialization.spaceSep @@ var "params")],
          var "body"]))),
      -- (defmacro name (params) body)
      L._Dialect_commonLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "defmacro", var "name"],
          list [Serialization.parens @@ (Serialization.spaceSep @@ var "params")],
          var "body"]))),
      -- (define-syntax name ...)
      L._Dialect_scheme>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "define-syntax", var "name"],
          var "body"])))]

-- | Serialize a map literal
mapLiteralToExpr :: TTermDefinition (L.Dialect -> L.MapLiteral -> Expr)
mapLiteralToExpr = define "mapLiteralToExpr" $
  lambda "d" $ lambda "ml" $ lets [
    "entries">: project L._MapLiteral L._MapLiteral_entries @@ var "ml"] $
    cases L._Dialect (var "d") Nothing [
      -- {:key1 val1 :key2 val2} in Clojure
      L._Dialect_clojure>>: constant $
        Serialization.brackets @@ (asTerm Serialization.curlyBraces) @@ (asTerm Serialization.inlineStyle) @@
          (Serialization.spaceSep @@ (Lists.concat (Lists.map (lambda "e" $ list [
            expressionToExpr @@ var "d" @@ (project L._MapEntry L._MapEntry_key @@ var "e"),
            expressionToExpr @@ var "d" @@ (project L._MapEntry L._MapEntry_value @@ var "e")])
            (var "entries")))),
      -- alist: '((key1 . val1) (key2 . val2)) in CL/EL — quoted because CL defstruct
      -- constructors take keyword args, and the bootstrap data uses positional calls
      L._Dialect_emacsLisp>>: constant $ mapAsAlist (var "d") (var "entries"),
      L._Dialect_commonLisp>>: constant $ mapAsAlist (var "d") (var "entries"),
      -- (list (cons key1 val1) (cons key2 val2) ...) in Scheme — evaluated, not quoted
      L._Dialect_scheme>>: constant $ mapAsConsExpressions (var "d") (var "entries")]
  where
    mapAsAlist :: TTerm L.Dialect -> TTerm [L.MapEntry] -> TTerm Expr
    mapAsAlist d entries =
      Serialization.noSep @@ list [
        Serialization.cst @@ string "'",
        Serialization.parens @@ (Serialization.spaceSep @@
          Lists.map (lambda "e" $
            Serialization.parens @@ (Serialization.spaceSep @@ list [
              expressionToExpr @@ d @@ (project L._MapEntry L._MapEntry_key @@ var "e"),
              Serialization.cst @@ string ".",
              expressionToExpr @@ d @@ (project L._MapEntry L._MapEntry_value @@ var "e")]))
            entries)]
    mapAsConsExpressions :: TTerm L.Dialect -> TTerm [L.MapEntry] -> TTerm Expr
    mapAsConsExpressions d entries =
      Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
        (list [Serialization.cst @@ string "list"])
        (Lists.map (lambda "e" $
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "cons",
            expressionToExpr @@ d @@ (project L._MapEntry L._MapEntry_key @@ var "e"),
            expressionToExpr @@ d @@ (project L._MapEntry L._MapEntry_value @@ var "e")]))
          entries))

-- | Serialize a module declaration
moduleDeclarationToExpr :: TTermDefinition (L.Dialect -> L.ModuleDeclaration -> Expr)
moduleDeclarationToExpr = define "moduleDeclarationToExpr" $
  lambda "d" $ lambda "mdecl" $ lets [
    "name">: unwrap L._NamespaceName @@ (project L._ModuleDeclaration L._ModuleDeclaration_name @@ var "mdecl")] $
    cases L._Dialect (var "d") Nothing [
      -- (ns name)
      L._Dialect_clojure>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "ns",
          Serialization.cst @@ var "name"]),
      -- (require 'cl-lib) (provide 'name)
      L._Dialect_emacsLisp>>: constant $
        Serialization.newlineSep @@ list [
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "require",
            Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ string "cl-lib"]]),
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "provide",
            Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ var "name"]])],
      -- (defpackage :name) (in-package :name)
      L._Dialect_commonLisp>>: constant $
        Serialization.newlineSep @@ list [
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "defpackage",
            Serialization.cst @@ Strings.cat2 (string ":") (var "name")]),
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "in-package",
            Serialization.cst @@ Strings.cat2 (string ":") (var "name")])],
      -- (define-library (name))
      L._Dialect_scheme>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "define-library",
          Serialization.parens @@ (Serialization.cst @@ var "name")])]

-- | Nil expression
nilExpr :: TTermDefinition (L.Dialect -> Expr)
nilExpr = define "nilExpr" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ Serialization.cst @@ string "nil",
    L._Dialect_emacsLisp>>: constant $ Serialization.cst @@ string "nil",
    L._Dialect_commonLisp>>: constant $ Serialization.cst @@ string "cl:nil",
    L._Dialect_scheme>>: constant $ Serialization.cst @@ string "'()"]

-- | Serialize a not expression: (not expr)
notExpressionToExpr :: TTermDefinition (L.Dialect -> L.NotExpression -> Expr)
notExpressionToExpr = define "notExpressionToExpr" $
  lambda "d" $ lambda "notExpr" $
    Serialization.parens @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "not",
      expressionToExpr @@ var "d" @@ (project L._NotExpression L._NotExpression_expression @@ var "notExpr")])

-- | Serialize an or expression: (or expr1 expr2 ...)
orExpressionToExpr :: TTermDefinition (L.Dialect -> L.OrExpression -> Expr)
orExpressionToExpr = define "orExpressionToExpr" $
  lambda "d" $ lambda "orExpr" $
    Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
      (list [Serialization.cst @@ string "or"])
      (Lists.map (expressionToExpr @@ var "d") (project L._OrExpression L._OrExpression_expressions @@ var "orExpr")))

-- | Serialize a full Lisp program
programToExpr :: TTermDefinition (L.Program -> Expr)
programToExpr = define "programToExpr" $
  lambda "prog" $ lets [
    "d">: project L._Program L._Program_dialect @@ var "prog",
    "modDecl">: project L._Program L._Program_module @@ var "prog",
    "imports">: project L._Program L._Program_imports @@ var "prog",
    "exports">: project L._Program L._Program_exports @@ var "prog",
    "forms">: project L._Program L._Program_forms @@ var "prog",
    "formPart">: Lists.map (topLevelFormWithCommentsToExpr @@ var "d") (var "forms"),
    -- Helper: get import module names as strings
    "importNames">: Lists.map (lambda "idecl" $
      unwrap L._NamespaceName @@ (project L._ImportDeclaration L._ImportDeclaration_module @@ var "idecl"))
      (var "imports"),
    -- Helper: get all export symbols as expr strings
    "exportSyms">: Lists.concat (Lists.map (lambda "edecl" $
      Lists.map symbolToExpr (project L._ExportDeclaration L._ExportDeclaration_symbols @@ var "edecl"))
      (var "exports"))] $
    cases L._Dialect (var "d") Nothing [
      -- Clojure: (ns name (:require [dep1 :refer :all] [dep2 :refer :all] ...)) then forms
      L._Dialect_clojure>>: constant $
        Maybes.maybe
          (Serialization.doubleNewlineSep @@ var "formPart")
          (lambda "m" $ lets [
            "nameStr">: unwrap L._NamespaceName @@ (project L._ModuleDeclaration L._ModuleDeclaration_name @@ var "m"),
            "requireClauses">: Lists.map (lambda "imp" $
              sqBrackets (list [
                Serialization.cst @@ var "imp",
                Serialization.cst @@ string ":refer",
                Serialization.cst @@ string ":all"]))
              (var "importNames"),
            "nsForm">: Logic.ifElse (Lists.null (var "requireClauses"))
              (Serialization.parens @@ (Serialization.spaceSep @@ list [
                Serialization.cst @@ string "ns",
                Serialization.cst @@ var "nameStr"]))
              (Serialization.parens @@ (Serialization.newlineSep @@ list [
                Serialization.spaceSep @@ list [
                  Serialization.cst @@ string "ns",
                  Serialization.cst @@ var "nameStr"],
                Serialization.spaceSep @@ Lists.concat2
                  (list [Serialization.cst @@ string "  (:require"])
                  (var "requireClauses"),
                Serialization.cst @@ string ")"]))] $
            -- Extract variable definition names for forward declaration
            "varNames" <~ Lists.concat (Lists.map (lambda "fwc" $
              lets ["form">: project L._TopLevelFormWithComments L._TopLevelFormWithComments_form @@ var "fwc"] $
              cases L._TopLevelForm (var "form") (Just (list ([] :: [TTerm Expr]))) [
                L._TopLevelForm_variable>>: lambda "vd" $
                  list [symbolToExpr @@ (project L._VariableDefinition L._VariableDefinition_name @@ var "vd")],
                L._TopLevelForm_function>>: lambda "fd" $
                  list [symbolToExpr @@ (project L._FunctionDefinition L._FunctionDefinition_name @@ var "fd")]])
              (var "forms")) $
            -- (declare name1 name2 ...) for forward references
            "declareForm" <~ Logic.ifElse (Lists.null (var "varNames"))
              (list ([] :: [TTerm Expr]))
              (list [Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
                (list [Serialization.cst @@ string "declare"])
                (var "varNames"))]) $
            Serialization.doubleNewlineSep @@ Lists.concat (list [
              list [var "nsForm"],
              var "declareForm",
              var "formPart"]))
          (var "modDecl"),
      -- Emacs Lisp: (require 'cl-lib) (require 'dep1) ... forms... (provide 'name)
      L._Dialect_emacsLisp>>: constant $
        Maybes.maybe
          (Serialization.doubleNewlineSep @@ var "formPart")
          (lambda "m" $ lets [
            "nameStr">: unwrap L._NamespaceName @@ (project L._ModuleDeclaration L._ModuleDeclaration_name @@ var "m"),
            "requireClLib">: Serialization.parens @@ (Serialization.spaceSep @@ list [
              Serialization.cst @@ string "require",
              Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ string "cl-lib"]]),
            "requireImports">: Lists.map (lambda "imp" $
              Serialization.parens @@ (Serialization.spaceSep @@ list [
                Serialization.cst @@ string "require",
                Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ var "imp"]]))
              (var "importNames"),
            "provideForm">: Serialization.parens @@ (Serialization.spaceSep @@ list [
              Serialization.cst @@ string "provide",
              Serialization.noSep @@ list [Serialization.cst @@ string "'", Serialization.cst @@ var "nameStr"]])] $
            Serialization.doubleNewlineSep @@ Lists.concat (list [
              list [var "requireClLib"],
              var "requireImports",
              var "formPart",
              list [var "provideForm"]]))
          (var "modDecl"),
      -- Common Lisp: (defpackage :name (:use :cl :dep1 :dep2 ...) (:export :sym1 :sym2 ...)) (in-package :name) forms...
      L._Dialect_commonLisp>>: constant $
        Maybes.maybe
          (Serialization.doubleNewlineSep @@ var "formPart")
          (lambda "m" $ lets [
            "nameStr">: unwrap L._NamespaceName @@ (project L._ModuleDeclaration L._ModuleDeclaration_name @@ var "m"),
            "colonName">: Strings.cat2 (string ":") (var "nameStr"),
            -- (:use :cl :dep1 :dep2 ...)
            "useClause">: Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
              (list [Serialization.cst @@ string ":use", Serialization.cst @@ string ":cl"])
              (Lists.map (lambda "imp" $ Serialization.cst @@ Strings.cat2 (string ":") (var "imp"))
                (var "importNames"))),
            -- (:export :sym1 :sym2 ...)
            "exportClause">: Logic.ifElse (Lists.null (var "exportSyms"))
              (list ([] :: [TTerm Expr]))
              (list [Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
                (list [Serialization.cst @@ string ":export"])
                (Lists.map (lambda "s" $ Serialization.noSep @@ list [Serialization.cst @@ string ":", var "s"]) (var "exportSyms")))]),
            -- (defpackage :name (:use ...) (:export ...))
            "defpkgForm">: Serialization.parens @@ (Serialization.newlineSep @@ Lists.concat (list [
              list [Serialization.spaceSep @@ list [
                Serialization.cst @@ string "defpackage",
                Serialization.cst @@ var "colonName"]],
              list [var "useClause"],
              var "exportClause"])),
            "inpkgForm">: Serialization.parens @@ (Serialization.spaceSep @@ list [
              Serialization.cst @@ string "in-package",
              Serialization.cst @@ var "colonName"])] $
            Serialization.doubleNewlineSep @@ Lists.concat (list [
              list [var "defpkgForm", var "inpkgForm"],
              var "formPart"]))
          (var "modDecl"),
      -- Scheme: wrap everything in (define-library (name ...) (export ...) (import ...) (begin ...))
      L._Dialect_scheme>>: constant $
        Maybes.maybe
          (Serialization.doubleNewlineSep @@ var "formPart")
          (lambda "m" $ lets [
            "nameStr">: unwrap L._NamespaceName @@ (project L._ModuleDeclaration L._ModuleDeclaration_name @@ var "m"),
            "nameParts">: Lists.map (lambda "p" $ Formatting.convertCaseCamelToLowerSnake @@ var "p")
              (Strings.splitOn (string ".") (var "nameStr")),
            "nameExpr">: Serialization.parens @@ (Serialization.spaceSep @@
              Lists.map (lambda "p" $ Serialization.cst @@ var "p") (var "nameParts")),
            -- Build import library names: split each namespace on dots for R7RS (import (hydra core) ...)
            "domainImportExprs">: Lists.map (lambda "idecl" $ lets [
              "nsName">: unwrap L._NamespaceName @@ (project L._ImportDeclaration L._ImportDeclaration_module @@ var "idecl"),
              "nsParts">: Lists.map (lambda "p" $ Formatting.convertCaseCamelToLowerSnake @@ var "p")
                (Strings.splitOn (string ".") (var "nsName"))] $
              Serialization.parens @@ (Serialization.spaceSep @@
                Lists.map (lambda "p" $ Serialization.cst @@ var "p") (var "nsParts")))
              (var "imports"),
            -- Combine (scheme base) + domain imports into a single (import ...) clause
            "schemeBaseExpr">: Serialization.parens @@ (Serialization.spaceSep @@ list [
              Serialization.cst @@ string "scheme",
              Serialization.cst @@ string "base"]),
            "allImportExprs">: Lists.concat2
              (list [var "schemeBaseExpr"])
              (var "domainImportExprs"),
            "importClause">: Serialization.parens @@ (Serialization.spaceSep @@
              Lists.concat2 (list [Serialization.cst @@ string "import"]) (var "allImportExprs")),
            -- Export clause (only if there are exports)
            "exportClauses">: Lists.map (lambda "edecl" $ exportDeclarationToExpr @@ var "d" @@ var "edecl") (var "exports"),
            "beginClause">: Serialization.parens @@ (Serialization.newlineSep @@
              Lists.concat2
                (list [Serialization.cst @@ string "begin"])
                (var "formPart"))] $
            Serialization.parens @@ (Serialization.newlineSep @@
              Lists.concat (list [
                list [Serialization.spaceSep @@ list [
                  Serialization.cst @@ string "define-library",
                  var "nameExpr"]],
                var "exportClauses",
                list [var "importClause"],
                list [var "beginClause"]])))
          (var "modDecl")]

-- | Serialize a record type definition
recordTypeDefinitionToExpr :: TTermDefinition (L.Dialect -> L.RecordTypeDefinition -> Expr)
recordTypeDefinitionToExpr = define "recordTypeDefinitionToExpr" $
  lambda "d" $ lambda "rdef" $ lets [
    "name">: symbolToExpr @@ (project L._RecordTypeDefinition L._RecordTypeDefinition_name @@ var "rdef"),
    "fields">: Lists.map (lambda "f" $ symbolToExpr @@ (project L._FieldDefinition L._FieldDefinition_name @@ var "f"))
      (project L._RecordTypeDefinition L._RecordTypeDefinition_fields @@ var "rdef")] $
    cases L._Dialect (var "d") Nothing [
      -- (defrecord Name [field1 field2])
      -- Also emit (defn make-Name [field1 field2] (->Name field1 field2)) for uniform constructor calls
      L._Dialect_clojure>>: constant $ lets [
        "nameStr">: unwrap L._Symbol @@ (project L._RecordTypeDefinition L._RecordTypeDefinition_name @@ var "rdef"),
        "fieldNames">: Lists.map (lambda "f" $ unwrap L._Symbol @@ (project L._FieldDefinition L._FieldDefinition_name @@ var "f"))
          (project L._RecordTypeDefinition L._RecordTypeDefinition_fields @@ var "rdef"),
        "defrecordForm">: Serialization.parens @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "defrecord",
          var "name",
          sqBrackets (var "fields")]),
        "makeAlias">: Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
          list [Serialization.cst @@ string "defn",
            Serialization.cst @@ Strings.cat2 (string "make-") (var "nameStr")],
          list [sqBrackets (var "fields")],
          list [Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
            (list [Serialization.cst @@ Strings.cat2 (string "->") (var "nameStr")])
            (Lists.map (lambda "fn" $ Serialization.cst @@ var "fn") (var "fieldNames")))]]))] $
        Serialization.newlineSep @@ list [var "defrecordForm", var "makeAlias"],
      -- (cl-defstruct name field1 field2)
      L._Dialect_emacsLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "cl-defstruct", var "name"],
          var "fields"]))),
      -- (cl:defstruct name field1 field2)
      L._Dialect_commonLisp>>: constant $
        Serialization.parens @@ (Serialization.spaceSep @@ (Lists.concat (list [
          list [Serialization.cst @@ string "cl:defstruct", var "name"],
          var "fields"]))),
      -- R7RS/SRFI-9: (define-record-type name (make-name f1 f2) name? (f1 name-f1) (f2 name-f2))
      -- The Coder passes snake_case names directly, so no case conversion is needed here.
      L._Dialect_scheme>>: constant $ lets [
        "nameStr">: unwrap L._Symbol @@ (project L._RecordTypeDefinition L._RecordTypeDefinition_name @@ var "rdef"),
        "fieldNames">: Lists.map (lambda "f" $ unwrap L._Symbol @@ (project L._FieldDefinition L._FieldDefinition_name @@ var "f"))
          (project L._RecordTypeDefinition L._RecordTypeDefinition_fields @@ var "rdef"),
        "constructor">: Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
          (list [Serialization.cst @@ Strings.cat2 (string "make-") (var "nameStr")])
          (Lists.map (lambda "fn" $ Serialization.cst @@ var "fn") (var "fieldNames"))),
        "predicate">: Serialization.cst @@ Strings.cat2 (var "nameStr") (string "?"),
        "accessors">: Lists.map (lambda "fn" $
          Serialization.parens @@ (Serialization.spaceSep @@ list [
            Serialization.cst @@ var "fn",
            Serialization.cst @@ Strings.cat (list [var "nameStr", string "-", var "fn"])]))
          (var "fieldNames")] $
        Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat (list [
          list [Serialization.cst @@ string "define-record-type", var "name", var "constructor", var "predicate"],
          var "accessors"]))]

-- | Serialize an S-expression (escape hatch)
sExpressionToExpr :: TTermDefinition (L.SExpression -> Expr)
sExpressionToExpr = define "sExpressionToExpr" $
  lambda "sexpr" $
    cases L._SExpression (var "sexpr") Nothing [
      L._SExpression_atom>>: lambda "a" $ Serialization.cst @@ var "a",
      L._SExpression_list>>: lambda "elems" $
        Serialization.parens @@ (Serialization.spaceSep @@ Lists.map sExpressionToExpr (var "elems"))]

-- | Serialize a set literal
setLiteralToExpr :: TTermDefinition (L.Dialect -> L.SetLiteral -> Expr)
setLiteralToExpr = define "setLiteralToExpr" $
  lambda "d" $ lambda "sl" $ lets [
    "elems">: Lists.map (expressionToExpr @@ var "d") (project L._SetLiteral L._SetLiteral_elements @@ var "sl")] $
    cases L._Dialect (var "d") Nothing [
      -- #{1 2 3} in Clojure
      L._Dialect_clojure>>: constant $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "#",
          Serialization.brackets @@ (asTerm Serialization.curlyBraces) @@ (asTerm Serialization.inlineStyle) @@
            (Serialization.spaceSep @@ var "elems")],
      -- (list->set (list 1 2 3)) as a fallback in others
      L._Dialect_emacsLisp>>: constant $ setAsList (string "list") (var "elems"),
      L._Dialect_commonLisp>>: constant $ setAsList (string "cl:list") (var "elems"),
      L._Dialect_scheme>>: constant $ setAsList (string "list") (var "elems")]
  where
    setAsList :: TTerm String -> TTerm [Expr] -> TTerm Expr
    setAsList kw elems =
      Serialization.parens @@ (Serialization.spaceSep @@ Lists.concat2
        (list [Serialization.cst @@ kw])
        elems)

-- | Serialize a symbol
symbolToExpr :: TTermDefinition (L.Symbol -> Expr)
symbolToExpr = define "symbolToExpr" $
  lambda "s" $ Serialization.cst @@ (unwrap L._Symbol @@ var "s")

-- | Serialize a top-level form
topLevelFormToExpr :: TTermDefinition (L.Dialect -> L.TopLevelForm -> Expr)
topLevelFormToExpr = define "topLevelFormToExpr" $
  lambda "d" $ lambda "form" $
    cases L._TopLevelForm (var "form") Nothing [
      L._TopLevelForm_function>>: lambda "f" $ functionDefinitionToExpr @@ var "d" @@ var "f",
      L._TopLevelForm_variable>>: lambda "v" $ variableDefinitionToExpr @@ var "d" @@ var "v",
      L._TopLevelForm_constant>>: lambda "c" $ constantDefinitionToExpr @@ var "d" @@ var "c",
      L._TopLevelForm_recordType>>: lambda "r" $ recordTypeDefinitionToExpr @@ var "d" @@ var "r",
      L._TopLevelForm_macro>>: lambda "m" $ macroDefinitionToExpr @@ var "d" @@ var "m",
      L._TopLevelForm_expression>>: lambda "e" $ expressionToExpr @@ var "d" @@ var "e"]

-- | Serialize a top-level form with comments
topLevelFormWithCommentsToExpr :: TTermDefinition (L.Dialect -> L.TopLevelFormWithComments -> Expr)
topLevelFormWithCommentsToExpr = define "topLevelFormWithCommentsToExpr" $
  lambda "d" $ lambda "fwc" $ lets [
    "mdoc">: project L._TopLevelFormWithComments L._TopLevelFormWithComments_doc @@ var "fwc",
    "mcomment">: project L._TopLevelFormWithComments L._TopLevelFormWithComments_comment @@ var "fwc",
    "form">: project L._TopLevelFormWithComments L._TopLevelFormWithComments_form @@ var "fwc",
    "docPart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "ds" $ list [docstringToExpr @@ var "ds"])
      (var "mdoc"),
    "commentPart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "c" $ list [commentToExpr @@ var "c"])
      (var "mcomment"),
    "formExpr">: topLevelFormToExpr @@ var "d" @@ var "form"] $
    Serialization.newlineSep @@ (Lists.concat (list [
      var "commentPart", var "docPart", list [var "formExpr"]]))

-- | Boolean true expression
trueExpr :: TTermDefinition (L.Dialect -> Expr)
trueExpr = define "trueExpr" $
  lambda "d" $ cases L._Dialect (var "d") Nothing [
    L._Dialect_clojure>>: constant $ Serialization.cst @@ string "true",
    L._Dialect_emacsLisp>>: constant $ Serialization.cst @@ string "t",
    L._Dialect_commonLisp>>: constant $ Serialization.cst @@ string "cl:t",
    L._Dialect_scheme>>: constant $ Serialization.cst @@ string "#t"]

-- | Serialize a variable definition
variableDefinitionToExpr :: TTermDefinition (L.Dialect -> L.VariableDefinition -> Expr)
variableDefinitionToExpr = define "variableDefinitionToExpr" $
  lambda "d" $ lambda "vdef" $ lets [
    "name">: symbolToExpr @@ (project L._VariableDefinition L._VariableDefinition_name @@ var "vdef"),
    "value">: expressionToExpr @@ var "d" @@ (project L._VariableDefinition L._VariableDefinition_value @@ var "vdef")] $
    Serialization.parens @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ (defKeyword @@ var "d"),
      var "name",
      var "value"])

-- | Serialize a variable reference
variableReferenceToExpr :: TTermDefinition (L.Dialect -> L.VariableReference -> Expr)
variableReferenceToExpr = define "variableReferenceToExpr" $
  lambda "d" $ lambda "vref" $ lets [
    "name">: symbolToExpr @@ (project L._VariableReference L._VariableReference_name @@ var "vref"),
    "isFnNs">: project L._VariableReference L._VariableReference_functionNamespace @@ var "vref"] $
    Logic.ifElse (var "isFnNs")
      (cases L._Dialect (var "d") Nothing [
        L._Dialect_commonLisp>>: constant $
          Serialization.noSep @@ list [Serialization.cst @@ string "#'", var "name"],
        L._Dialect_clojure>>: constant $ var "name",
        L._Dialect_emacsLisp>>: constant $ var "name",
        L._Dialect_scheme>>: constant $ var "name"])
      (var "name")

-- | Serialize a vector literal
vectorLiteralToExpr :: TTermDefinition (L.Dialect -> L.VectorLiteral -> Expr)
vectorLiteralToExpr = define "vectorLiteralToExpr" $
  lambda "d" $ lambda "vl" $ lets [
    "elems">: Lists.map (expressionToExpr @@ var "d") (project L._VectorLiteral L._VectorLiteral_elements @@ var "vl")] $
    cases L._Dialect (var "d") Nothing [
      -- [1 2 3] in Clojure and Emacs Lisp
      L._Dialect_clojure>>: constant $ sqBrackets (var "elems"),
      L._Dialect_emacsLisp>>: constant $ sqBrackets (var "elems"),
      -- #(1 2 3) in Common Lisp and Scheme
      L._Dialect_commonLisp>>: constant $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "#",
          Serialization.parens @@ (Serialization.spaceSep @@ var "elems")],
      L._Dialect_scheme>>: constant $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "#",
          Serialization.parens @@ (Serialization.spaceSep @@ var "elems")]]
