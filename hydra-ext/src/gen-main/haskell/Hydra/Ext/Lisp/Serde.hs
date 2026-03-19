-- Note: this is an automatically generated file. Do not edit.

-- | Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme

module Hydra.Ext.Lisp.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Lisp.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

defnKeyword :: Syntax.Dialect -> String
defnKeyword d =
    case d of
      Syntax.DialectClojure -> "defn"
      Syntax.DialectEmacsLisp -> "defun"
      Syntax.DialectCommonLisp -> "cl:defun"
      Syntax.DialectScheme -> "define"

defKeyword :: Syntax.Dialect -> String
defKeyword d =
    case d of
      Syntax.DialectClojure -> "def"
      Syntax.DialectEmacsLisp -> "defvar"
      Syntax.DialectCommonLisp -> "cl:defvar"
      Syntax.DialectScheme -> "define"

defconstKeyword :: Syntax.Dialect -> String
defconstKeyword d =
    case d of
      Syntax.DialectClojure -> "def"
      Syntax.DialectEmacsLisp -> "defconst"
      Syntax.DialectCommonLisp -> "cl:defconstant"
      Syntax.DialectScheme -> "define"

defrecordKeyword :: Syntax.Dialect -> String
defrecordKeyword d =
    case d of
      Syntax.DialectClojure -> "defrecord"
      Syntax.DialectEmacsLisp -> "cl-defstruct"
      Syntax.DialectCommonLisp -> "cl:defstruct"
      Syntax.DialectScheme -> "define-record-type"

lambdaKeyword :: Syntax.Dialect -> String
lambdaKeyword d =
    case d of
      Syntax.DialectClojure -> "fn"
      Syntax.DialectEmacsLisp -> "lambda"
      Syntax.DialectCommonLisp -> "cl:lambda"
      Syntax.DialectScheme -> "lambda"

listKeyword :: Syntax.Dialect -> String
listKeyword d =
    case d of
      Syntax.DialectClojure -> "list"
      Syntax.DialectEmacsLisp -> "list"
      Syntax.DialectCommonLisp -> "cl:list"
      Syntax.DialectScheme -> "list"

trueExpr :: Syntax.Dialect -> Ast.Expr
trueExpr d =
    case d of
      Syntax.DialectClojure -> Serialization.cst "true"
      Syntax.DialectEmacsLisp -> Serialization.cst "t"
      Syntax.DialectCommonLisp -> Serialization.cst "cl:t"
      Syntax.DialectScheme -> Serialization.cst "#t"

falseExpr :: Syntax.Dialect -> Ast.Expr
falseExpr d =
    case d of
      Syntax.DialectClojure -> Serialization.cst "false"
      Syntax.DialectEmacsLisp -> Serialization.cst "nil"
      Syntax.DialectCommonLisp -> Serialization.cst "cl:nil"
      Syntax.DialectScheme -> Serialization.cst "#f"

nilExpr :: Syntax.Dialect -> Ast.Expr
nilExpr d =
    case d of
      Syntax.DialectClojure -> Serialization.cst "nil"
      Syntax.DialectEmacsLisp -> Serialization.cst "nil"
      Syntax.DialectCommonLisp -> Serialization.cst "cl:nil"
      Syntax.DialectScheme -> Serialization.cst "'()"

programToExpr :: Syntax.Program -> Ast.Expr
programToExpr prog =

      let d = Syntax.programDialect prog
          modDecl = Syntax.programModule prog
          imports = Syntax.programImports prog
          exports = Syntax.programExports prog
          forms = Syntax.programForms prog
          formPart = Lists.map (topLevelFormWithCommentsToExpr d) forms
          importNames = Lists.map (\idecl -> Syntax.unNamespaceName (Syntax.importDeclarationModule idecl)) imports
          exportSyms = Lists.concat (Lists.map (\edecl -> Lists.map symbolToExpr (Syntax.exportDeclarationSymbols edecl)) exports)
      in case d of
        Syntax.DialectClojure -> Maybes.maybe (Serialization.doubleNewlineSep formPart) (\m ->
          let nameStr = Syntax.unNamespaceName (Syntax.moduleDeclarationName m)
              requireClauses =
                      Lists.map (\imp -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep [
                        Serialization.cst imp,
                        (Serialization.cst ":refer"),
                        (Serialization.cst ":all")])) importNames
              nsForm =
                      Logic.ifElse (Lists.null requireClauses) (Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "ns",
                        (Serialization.cst nameStr)])) (Serialization.parens (Serialization.newlineSep [
                        Serialization.spaceSep [
                          Serialization.cst "ns",
                          (Serialization.cst nameStr)],
                        (Serialization.spaceSep (Lists.concat2 [
                          Serialization.cst "  (:require"] requireClauses)),
                        (Serialization.cst ")")]))
              varNames =
                      Lists.concat (Lists.map (\fwc ->
                        let form = Syntax.topLevelFormWithCommentsForm fwc
                        in case form of
                          Syntax.TopLevelFormVariable v1 -> [
                            symbolToExpr (Syntax.variableDefinitionName v1)]
                          Syntax.TopLevelFormFunction v1 -> [
                            symbolToExpr (Syntax.functionDefinitionName v1)]
                          _ -> []) forms)
              declareForm =
                      Logic.ifElse (Lists.null varNames) [] [
                        Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                          Serialization.cst "declare"] varNames))]
          in (Serialization.doubleNewlineSep (Lists.concat [
            [
              nsForm],
            declareForm,
            formPart]))) modDecl
        Syntax.DialectEmacsLisp -> Maybes.maybe (Serialization.doubleNewlineSep formPart) (\m ->
          let nameStr = Syntax.unNamespaceName (Syntax.moduleDeclarationName m)
              requireClLib =
                      Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "require",
                        (Serialization.noSep [
                          Serialization.cst "'",
                          (Serialization.cst "cl-lib")])])
              requireImports =
                      Lists.map (\imp -> Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "require",
                        (Serialization.noSep [
                          Serialization.cst "'",
                          (Serialization.cst imp)])])) importNames
              provideForm =
                      Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "provide",
                        (Serialization.noSep [
                          Serialization.cst "'",
                          (Serialization.cst nameStr)])])
          in (Serialization.doubleNewlineSep (Lists.concat [
            [
              requireClLib],
            requireImports,
            formPart,
            [
              provideForm]]))) modDecl
        Syntax.DialectCommonLisp -> Maybes.maybe (Serialization.doubleNewlineSep formPart) (\m ->
          let nameStr = Syntax.unNamespaceName (Syntax.moduleDeclarationName m)
              colonName = Strings.cat2 ":" nameStr
              useClause =
                      Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                        Serialization.cst ":use",
                        (Serialization.cst ":cl")] (Lists.map (\imp -> Serialization.cst (Strings.cat2 ":" imp)) importNames)))
              exportClause =
                      Logic.ifElse (Lists.null exportSyms) [] [
                        Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                          Serialization.cst ":export"] (Lists.map (\s -> Serialization.noSep [
                          Serialization.cst ":",
                          s]) exportSyms)))]
              defpkgForm =
                      Serialization.parens (Serialization.newlineSep (Lists.concat [
                        [
                          Serialization.spaceSep [
                            Serialization.cst "defpackage",
                            (Serialization.cst colonName)]],
                        [
                          useClause],
                        exportClause]))
              inpkgForm =
                      Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "in-package",
                        (Serialization.cst colonName)])
          in (Serialization.doubleNewlineSep (Lists.concat [
            [
              defpkgForm,
              inpkgForm],
            formPart]))) modDecl
        Syntax.DialectScheme -> Maybes.maybe (Serialization.doubleNewlineSep formPart) (\m ->
          let nameStr = Syntax.unNamespaceName (Syntax.moduleDeclarationName m)
              nameParts = Lists.map (\p -> Formatting.convertCaseCamelToLowerSnake p) (Strings.splitOn "." nameStr)
              nameExpr = Serialization.parens (Serialization.spaceSep (Lists.map (\p -> Serialization.cst p) nameParts))
              domainImportExprs =
                      Lists.map (\idecl ->
                        let nsName = Syntax.unNamespaceName (Syntax.importDeclarationModule idecl)
                            nsParts = Lists.map (\p -> Formatting.convertCaseCamelToLowerSnake p) (Strings.splitOn "." nsName)
                        in (Serialization.parens (Serialization.spaceSep (Lists.map (\p -> Serialization.cst p) nsParts)))) imports
              schemeBaseExpr =
                      Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "scheme",
                        (Serialization.cst "base")])
              allImportExprs = Lists.concat2 [
                    schemeBaseExpr] domainImportExprs
              importClause = Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                    Serialization.cst "import"] allImportExprs))
              exportClauses = Lists.map (\edecl -> exportDeclarationToExpr d edecl) exports
              beginClause = Serialization.parens (Serialization.newlineSep (Lists.concat2 [
                    Serialization.cst "begin"] formPart))
          in (Serialization.parens (Serialization.newlineSep (Lists.concat [
            [
              Serialization.spaceSep [
                Serialization.cst "define-library",
                nameExpr]],
            exportClauses,
            [
              importClause],
            [
              beginClause]])))) modDecl

topLevelFormWithCommentsToExpr :: Syntax.Dialect -> Syntax.TopLevelFormWithComments -> Ast.Expr
topLevelFormWithCommentsToExpr d fwc =

      let mdoc = Syntax.topLevelFormWithCommentsDoc fwc
          mcomment = Syntax.topLevelFormWithCommentsComment fwc
          form = Syntax.topLevelFormWithCommentsForm fwc
          docPart = Maybes.maybe [] (\ds -> [
                docstringToExpr ds]) mdoc
          commentPart = Maybes.maybe [] (\c -> [
                commentToExpr c]) mcomment
          formExpr = topLevelFormToExpr d form
      in (Serialization.newlineSep (Lists.concat [
        commentPart,
        docPart,
        [
          formExpr]]))

topLevelFormToExpr :: Syntax.Dialect -> Syntax.TopLevelForm -> Ast.Expr
topLevelFormToExpr d form =
    case form of
      Syntax.TopLevelFormFunction v0 -> functionDefinitionToExpr d v0
      Syntax.TopLevelFormVariable v0 -> variableDefinitionToExpr d v0
      Syntax.TopLevelFormConstant v0 -> constantDefinitionToExpr d v0
      Syntax.TopLevelFormRecordType v0 -> recordTypeDefinitionToExpr d v0
      Syntax.TopLevelFormMacro v0 -> macroDefinitionToExpr d v0
      Syntax.TopLevelFormExpression v0 -> expressionToExpr d v0

functionDefinitionToExpr :: Syntax.Dialect -> Syntax.FunctionDefinition -> Ast.Expr
functionDefinitionToExpr d fdef =

      let name = symbolToExpr (Syntax.functionDefinitionName fdef)
          params = Lists.map symbolToExpr (Syntax.functionDefinitionParams fdef)
          body = Lists.map (expressionToExpr d) (Syntax.functionDefinitionBody fdef)
      in case d of
        Syntax.DialectClojure -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defn",
            name],
          [
            Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep params)],
          body]))
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defun",
            name],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defun",
            name],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "define"],
          [
            Serialization.parens (Serialization.spaceSep (Lists.concat2 [
              name] params))],
          body]))

variableDefinitionToExpr :: Syntax.Dialect -> Syntax.VariableDefinition -> Ast.Expr
variableDefinitionToExpr d vdef =

      let name = symbolToExpr (Syntax.variableDefinitionName vdef)
          value = expressionToExpr d (Syntax.variableDefinitionValue vdef)
      in (Serialization.parens (Serialization.spaceSep [
        Serialization.cst (defKeyword d),
        name,
        value]))

constantDefinitionToExpr :: Syntax.Dialect -> Syntax.ConstantDefinition -> Ast.Expr
constantDefinitionToExpr d cdef =

      let name = symbolToExpr (Syntax.constantDefinitionName cdef)
          value = expressionToExpr d (Syntax.constantDefinitionValue cdef)
      in (Serialization.parens (Serialization.spaceSep [
        Serialization.cst (defconstKeyword d),
        name,
        value]))

recordTypeDefinitionToExpr :: Syntax.Dialect -> Syntax.RecordTypeDefinition -> Ast.Expr
recordTypeDefinitionToExpr d rdef =

      let name = symbolToExpr (Syntax.recordTypeDefinitionName rdef)
          fields = Lists.map (\f -> symbolToExpr (Syntax.fieldDefinitionName f)) (Syntax.recordTypeDefinitionFields rdef)
      in case d of
        Syntax.DialectClojure ->
          let nameStr = Syntax.unSymbol (Syntax.recordTypeDefinitionName rdef)
              fieldNames = Lists.map (\f -> Syntax.unSymbol (Syntax.fieldDefinitionName f)) (Syntax.recordTypeDefinitionFields rdef)
              defrecordForm =
                      Serialization.parens (Serialization.spaceSep [
                        Serialization.cst "defrecord",
                        name,
                        (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep fields))])
              makeAlias =
                      Serialization.parens (Serialization.spaceSep (Lists.concat [
                        [
                          Serialization.cst "defn",
                          (Serialization.cst (Strings.cat2 "make-" nameStr))],
                        [
                          Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep fields)],
                        [
                          Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                            Serialization.cst (Strings.cat2 "->" nameStr)] (Lists.map (\fn -> Serialization.cst fn) fieldNames)))]]))
          in (Serialization.newlineSep [
            defrecordForm,
            makeAlias])
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "cl-defstruct",
            name],
          fields]))
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "cl:defstruct",
            name],
          fields]))
        Syntax.DialectScheme ->
          let nameStr = Syntax.unSymbol (Syntax.recordTypeDefinitionName rdef)
              fieldNames = Lists.map (\f -> Syntax.unSymbol (Syntax.fieldDefinitionName f)) (Syntax.recordTypeDefinitionFields rdef)
              constructor =
                      Serialization.parens (Serialization.spaceSep (Lists.concat2 [
                        Serialization.cst (Strings.cat2 "make-" nameStr)] (Lists.map (\fn -> Serialization.cst fn) fieldNames)))
              predicate = Serialization.cst (Strings.cat2 nameStr "?")
              accessors =
                      Lists.map (\fn -> Serialization.parens (Serialization.spaceSep [
                        Serialization.cst fn,
                        (Serialization.cst (Strings.cat [
                          nameStr,
                          "-",
                          fn]))])) fieldNames
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "define-record-type",
              name,
              constructor,
              predicate],
            accessors])))

macroDefinitionToExpr :: Syntax.Dialect -> Syntax.MacroDefinition -> Ast.Expr
macroDefinitionToExpr d mdef =

      let name = symbolToExpr (Syntax.macroDefinitionName mdef)
          params = Lists.map symbolToExpr (Syntax.macroDefinitionParams mdef)
          body = Lists.map (expressionToExpr d) (Syntax.macroDefinitionBody mdef)
      in case d of
        Syntax.DialectClojure -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defmacro",
            name],
          [
            Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep params)],
          body]))
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defmacro",
            name],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "defmacro",
            name],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst "define-syntax",
            name],
          body]))

expressionToExpr :: Syntax.Dialect -> Syntax.Expression -> Ast.Expr
expressionToExpr d expr =
    case expr of
      Syntax.ExpressionApplication v0 -> applicationToExpr d v0
      Syntax.ExpressionLambda v0 -> lambdaToExpr d v0
      Syntax.ExpressionLet v0 -> letExpressionToExpr d v0
      Syntax.ExpressionIf v0 -> ifExpressionToExpr d v0
      Syntax.ExpressionCond v0 -> condExpressionToExpr d v0
      Syntax.ExpressionCase v0 -> caseExpressionToExpr d v0
      Syntax.ExpressionAnd v0 -> andExpressionToExpr d v0
      Syntax.ExpressionOr v0 -> orExpressionToExpr d v0
      Syntax.ExpressionNot v0 -> notExpressionToExpr d v0
      Syntax.ExpressionDo v0 -> doExpressionToExpr d v0
      Syntax.ExpressionBegin v0 -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
        Serialization.cst "begin"] (Lists.map (expressionToExpr d) (Syntax.beginExpressionExpressions v0))))
      Syntax.ExpressionVariable v0 -> variableReferenceToExpr d v0
      Syntax.ExpressionLiteral v0 -> literalToExpr d v0
      Syntax.ExpressionList v0 -> listLiteralToExpr d v0
      Syntax.ExpressionVector v0 -> vectorLiteralToExpr d v0
      Syntax.ExpressionMap v0 -> mapLiteralToExpr d v0
      Syntax.ExpressionSet v0 -> setLiteralToExpr d v0
      Syntax.ExpressionCons v0 -> Serialization.parens (Serialization.spaceSep [
        Serialization.cst "cons",
        (expressionToExpr d (Syntax.consExpressionHead v0)),
        (expressionToExpr d (Syntax.consExpressionTail v0))])
      Syntax.ExpressionDottedPair v0 -> Serialization.parens (Serialization.spaceSep [
        expressionToExpr d (Syntax.dottedPairCar v0),
        (Serialization.cst "."),
        (expressionToExpr d (Syntax.dottedPairCdr v0))])
      Syntax.ExpressionFieldAccess v0 -> fieldAccessToExpr d v0
      Syntax.ExpressionTypeAnnotation v0 -> expressionToExpr d (Syntax.typeAnnotationExpression v0)
      Syntax.ExpressionQuote v0 -> Serialization.noSep [
        Serialization.cst "'",
        (expressionToExpr d (Syntax.quoteExpressionBody v0))]
      Syntax.ExpressionQuasiquote v0 -> Serialization.noSep [
        Serialization.cst "`",
        (expressionToExpr d (Syntax.quasiquoteExpressionBody v0))]
      Syntax.ExpressionUnquote v0 -> case d of
        Syntax.DialectClojure -> Serialization.noSep [
          Serialization.cst "~",
          (expressionToExpr d (Syntax.unquoteExpressionBody v0))]
        Syntax.DialectEmacsLisp -> Serialization.noSep [
          Serialization.cst ",",
          (expressionToExpr d (Syntax.unquoteExpressionBody v0))]
        Syntax.DialectCommonLisp -> Serialization.noSep [
          Serialization.cst ",",
          (expressionToExpr d (Syntax.unquoteExpressionBody v0))]
        Syntax.DialectScheme -> Serialization.noSep [
          Serialization.cst ",",
          (expressionToExpr d (Syntax.unquoteExpressionBody v0))]
      Syntax.ExpressionSplicingUnquote v0 -> case d of
        Syntax.DialectClojure -> Serialization.noSep [
          Serialization.cst "~@",
          (expressionToExpr d (Syntax.splicingUnquoteExpressionBody v0))]
        Syntax.DialectEmacsLisp -> Serialization.noSep [
          Serialization.cst ",@",
          (expressionToExpr d (Syntax.splicingUnquoteExpressionBody v0))]
        Syntax.DialectCommonLisp -> Serialization.noSep [
          Serialization.cst ",@",
          (expressionToExpr d (Syntax.splicingUnquoteExpressionBody v0))]
        Syntax.DialectScheme -> Serialization.noSep [
          Serialization.cst ",@",
          (expressionToExpr d (Syntax.splicingUnquoteExpressionBody v0))]
      Syntax.ExpressionSExpression v0 -> sExpressionToExpr v0

applicationToExpr :: Syntax.Dialect -> Syntax.Application -> Ast.Expr
applicationToExpr d app =

      let funExpr = Syntax.applicationFunction app
          fun = expressionToExpr d funExpr
          args = Lists.map (expressionToExpr d) (Syntax.applicationArguments app)
          needsFuncall = case d of
            Syntax.DialectEmacsLisp -> case funExpr of
              Syntax.ExpressionVariable _ -> False
              _ -> True
            _ -> False
          allParts = if needsFuncall
            then Lists.concat2 [Serialization.cst "funcall", fun] args
            else Lists.concat2 [fun] args
      in (Serialization.parens (Serialization.spaceSep allParts))

lambdaToExpr :: Syntax.Dialect -> Syntax.Lambda -> Ast.Expr
lambdaToExpr d lam =

      let params = Lists.map symbolToExpr (Syntax.lambdaParams lam)
          body = Lists.map (expressionToExpr d) (Syntax.lambdaBody lam)
          mname = Syntax.lambdaName lam
          kw = lambdaKeyword d
      in case d of
        Syntax.DialectClojure -> Maybes.maybe (Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst kw],
          [
            Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep params)],
          body]))) (\sym -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst kw,
            (symbolToExpr sym)],
          [
            Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep params)],
          body]))) mname
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst kw],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst kw],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            Serialization.cst kw],
          [
            Serialization.parens (Serialization.spaceSep params)],
          body]))

letExpressionToExpr :: Syntax.Dialect -> Syntax.LetExpression -> Ast.Expr
letExpressionToExpr d letExpr =

      let kind = Syntax.letExpressionKind letExpr
          bindings = Syntax.letExpressionBindings letExpr
          body = Lists.map (expressionToExpr d) (Syntax.letExpressionBody letExpr)
          bindingPairs =
                  Lists.map (\b -> case b of
                    Syntax.LetBindingSimple v0 -> (symbolToExpr (Syntax.simpleBindingName v0), (expressionToExpr d (Syntax.simpleBindingValue v0)))
                    Syntax.LetBindingDestructuring _ -> (Serialization.cst "<destructuring>", (Serialization.cst "<destructuring>"))) bindings
      in case d of
        Syntax.DialectClojure -> case kind of
          Syntax.LetKindRecursive -> Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "let"],
            [
              Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep (Lists.concat (Lists.map (\p -> [
                Pairs.first p,
                (Pairs.second p)]) bindingPairs)))],
            body]))
          Syntax.LetKindParallel -> Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "let"],
            [
              Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep (Lists.concat (Lists.map (\p -> [
                Pairs.first p,
                (Pairs.second p)]) bindingPairs)))],
            body]))
          Syntax.LetKindSequential -> Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "let"],
            [
              Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep (Lists.concat (Lists.map (\p -> [
                Pairs.first p,
                (Pairs.second p)]) bindingPairs)))],
            body]))
        Syntax.DialectEmacsLisp ->
          let kw =
                  case kind of
                    Syntax.LetKindParallel -> "let"
                    Syntax.LetKindSequential -> "let*"
                    Syntax.LetKindRecursive -> "letrec"
              bindingExprs =
                      Lists.map (\p -> Serialization.parens (Serialization.spaceSep [
                        Pairs.first p,
                        (Pairs.second p)])) bindingPairs
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst kw],
            [
              Serialization.parens (Serialization.spaceSep bindingExprs)],
            body])))
        Syntax.DialectCommonLisp ->
          let kw =
                  case kind of
                    Syntax.LetKindParallel -> "let"
                    Syntax.LetKindSequential -> "let*"
                    Syntax.LetKindRecursive -> "letrec"
              bindingExprs =
                      Lists.map (\p -> Serialization.parens (Serialization.spaceSep [
                        Pairs.first p,
                        (Pairs.second p)])) bindingPairs
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst kw],
            [
              Serialization.parens (Serialization.spaceSep bindingExprs)],
            body])))
        Syntax.DialectScheme ->
          let kw =
                  case kind of
                    Syntax.LetKindParallel -> "let"
                    Syntax.LetKindSequential -> "let*"
                    Syntax.LetKindRecursive -> "letrec"
              bindingExprs =
                      Lists.map (\p -> Serialization.parens (Serialization.spaceSep [
                        Pairs.first p,
                        (Pairs.second p)])) bindingPairs
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst kw],
            [
              Serialization.parens (Serialization.spaceSep bindingExprs)],
            body])))

ifExpressionToExpr :: Syntax.Dialect -> Syntax.IfExpression -> Ast.Expr
ifExpressionToExpr d ifExpr =

      let cond = expressionToExpr d (Syntax.ifExpressionCondition ifExpr)
          then_ = expressionToExpr d (Syntax.ifExpressionThen ifExpr)
          else_ = Syntax.ifExpressionElse ifExpr
          elsePart = Maybes.maybe [] (\e -> [
                expressionToExpr d e]) else_
      in (Serialization.parens (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "if",
          cond,
          then_],
        elsePart])))

condExpressionToExpr :: Syntax.Dialect -> Syntax.CondExpression -> Ast.Expr
condExpressionToExpr d condExpr =

      let clauses = Syntax.condExpressionClauses condExpr
          dflt = Syntax.condExpressionDefault condExpr
      in case d of
        Syntax.DialectClojure ->
          let clauseExprs =
                  Lists.concat (Lists.map (\c -> [
                    expressionToExpr d (Syntax.condClauseCondition c),
                    (expressionToExpr d (Syntax.condClauseBody c))]) clauses)
              defaultPart =
                      Maybes.maybe [] (\e -> [
                        Serialization.cst ":else",
                        (expressionToExpr d e)]) dflt
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "cond"],
            clauseExprs,
            defaultPart])))
        Syntax.DialectEmacsLisp ->
          let clauseExprs =
                  Lists.map (\c -> Serialization.parens (Serialization.spaceSep [
                    expressionToExpr d (Syntax.condClauseCondition c),
                    (expressionToExpr d (Syntax.condClauseBody c))])) clauses
              defaultPart =
                      Maybes.maybe [] (\e -> [
                        Serialization.parens (Serialization.spaceSep [
                          Serialization.cst "t",
                          (expressionToExpr d e)])]) dflt
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "cond"],
            clauseExprs,
            defaultPart])))
        Syntax.DialectCommonLisp ->
          let clauseExprs =
                  Lists.map (\c -> Serialization.parens (Serialization.spaceSep [
                    expressionToExpr d (Syntax.condClauseCondition c),
                    (expressionToExpr d (Syntax.condClauseBody c))])) clauses
              defaultPart =
                      Maybes.maybe [] (\e -> [
                        Serialization.parens (Serialization.spaceSep [
                          Serialization.cst "t",
                          (expressionToExpr d e)])]) dflt
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "cond"],
            clauseExprs,
            defaultPart])))
        Syntax.DialectScheme ->
          let clauseExprs =
                  Lists.map (\c -> Serialization.parens (Serialization.spaceSep [
                    expressionToExpr d (Syntax.condClauseCondition c),
                    (expressionToExpr d (Syntax.condClauseBody c))])) clauses
              defaultPart =
                      Maybes.maybe [] (\e -> [
                        Serialization.parens (Serialization.spaceSep [
                          Serialization.cst "else",
                          (expressionToExpr d e)])]) dflt
          in (Serialization.parens (Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "cond"],
            clauseExprs,
            defaultPart])))

caseExpressionToExpr :: Syntax.Dialect -> Syntax.CaseExpression -> Ast.Expr
caseExpressionToExpr d caseExpr =

      let scrutinee = expressionToExpr d (Syntax.caseExpressionScrutinee caseExpr)
          clauses = Syntax.caseExpressionClauses caseExpr
          dflt = Syntax.caseExpressionDefault caseExpr
          clauseExprs =
                  Lists.map (\c -> Serialization.parens (Serialization.spaceSep [
                    Serialization.parens (Serialization.spaceSep (Lists.map (expressionToExpr d) (Syntax.caseClauseKeys c))),
                    (expressionToExpr d (Syntax.caseClauseBody c))])) clauses
          defaultPart =
                  Maybes.maybe [] (\e -> [
                    Serialization.parens (Serialization.spaceSep [
                      Serialization.cst "else",
                      (expressionToExpr d e)])]) dflt
      in (Serialization.parens (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "case",
          scrutinee],
        clauseExprs,
        defaultPart])))

andExpressionToExpr :: Syntax.Dialect -> Syntax.AndExpression -> Ast.Expr
andExpressionToExpr d andExpr =
    Serialization.parens (Serialization.spaceSep (Lists.concat2 [
      Serialization.cst "and"] (Lists.map (expressionToExpr d) (Syntax.andExpressionExpressions andExpr))))

orExpressionToExpr :: Syntax.Dialect -> Syntax.OrExpression -> Ast.Expr
orExpressionToExpr d orExpr =
    Serialization.parens (Serialization.spaceSep (Lists.concat2 [
      Serialization.cst "or"] (Lists.map (expressionToExpr d) (Syntax.orExpressionExpressions orExpr))))

notExpressionToExpr :: Syntax.Dialect -> Syntax.NotExpression -> Ast.Expr
notExpressionToExpr d notExpr =
    Serialization.parens (Serialization.spaceSep [
      Serialization.cst "not",
      (expressionToExpr d (Syntax.notExpressionExpression notExpr))])

doExpressionToExpr :: Syntax.Dialect -> Syntax.DoExpression -> Ast.Expr
doExpressionToExpr d doExpr =

      let kw =
              case d of
                Syntax.DialectClojure -> "do"
                Syntax.DialectEmacsLisp -> "progn"
                Syntax.DialectCommonLisp -> "progn"
                Syntax.DialectScheme -> "begin"
      in (Serialization.parens (Serialization.spaceSep (Lists.concat2 [
        Serialization.cst kw] (Lists.map (expressionToExpr d) (Syntax.doExpressionExpressions doExpr)))))

variableReferenceToExpr :: Syntax.Dialect -> Syntax.VariableReference -> Ast.Expr
variableReferenceToExpr d vref =

      let name = symbolToExpr (Syntax.variableReferenceName vref)
          isFnNs = Syntax.variableReferenceFunctionNamespace vref
      in (Logic.ifElse isFnNs (case d of
        Syntax.DialectCommonLisp -> Serialization.noSep [
          Serialization.cst "#'",
          name]
        Syntax.DialectClojure -> name
        Syntax.DialectEmacsLisp -> name
        Syntax.DialectScheme -> name) name)

fieldAccessToExpr :: Syntax.Dialect -> Syntax.FieldAccess -> Ast.Expr
fieldAccessToExpr d fa =

      let rtype = symbolToExpr (Syntax.fieldAccessRecordType fa)
          field = symbolToExpr (Syntax.fieldAccessField fa)
          target = expressionToExpr d (Syntax.fieldAccessTarget fa)
      in case d of
        Syntax.DialectClojure -> Serialization.parens (Serialization.spaceSep [
          Serialization.noSep [
            Serialization.cst ":",
            field],
          target])
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep [
          Serialization.noSep [
            rtype,
            (Serialization.cst "-"),
            field],
          target])
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep [
          Serialization.noSep [
            rtype,
            (Serialization.cst "-"),
            field],
          target])
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep [
          Serialization.noSep [
            rtype,
            (Serialization.cst "-"),
            field],
          target])

literalToExpr :: Syntax.Dialect -> Syntax.Literal -> Ast.Expr
literalToExpr d lit =
    case lit of
      Syntax.LiteralInteger v0 -> Serialization.cst (Literals.showBigint (Syntax.integerLiteralValue v0))
      Syntax.LiteralFloat v0 -> Serialization.cst (Literals.showBigfloat (Syntax.floatLiteralValue v0))
      Syntax.LiteralString v0 ->
        let e1 = Strings.intercalate "\\\\" (Strings.splitOn "\\" v0)
        in case d of
          Syntax.DialectCommonLisp ->
            let escaped = Strings.intercalate "\\\"" (Strings.splitOn "\"" e1)
            in (Serialization.cst (Strings.cat [
              "\"",
              escaped,
              "\""]))
          Syntax.DialectClojure ->
            let e2 = Strings.intercalate "\\n" (Strings.splitOn (Strings.fromList [
                  10]) e1)
                e3 = Strings.intercalate "\\r" (Strings.splitOn (Strings.fromList [
                      13]) e2)
                e4 = Strings.intercalate "\\t" (Strings.splitOn (Strings.fromList [
                      9]) e3)
                escaped = Strings.intercalate "\\\"" (Strings.splitOn "\"" e4)
            in (Serialization.cst (Strings.cat [
              "\"",
              escaped,
              "\""]))
          Syntax.DialectEmacsLisp ->
            let e2 = Strings.intercalate "\\n" (Strings.splitOn (Strings.fromList [
                  10]) e1)
                e3 = Strings.intercalate "\\r" (Strings.splitOn (Strings.fromList [
                      13]) e2)
                e4 = Strings.intercalate "\\t" (Strings.splitOn (Strings.fromList [
                      9]) e3)
                escaped = Strings.intercalate "\\\"" (Strings.splitOn "\"" e4)
            in (Serialization.cst (Strings.cat [
              "\"",
              escaped,
              "\""]))
          Syntax.DialectScheme ->
            let e2 = Strings.intercalate "\\n" (Strings.splitOn (Strings.fromList [
                  10]) e1)
                e3 = Strings.intercalate "\\r" (Strings.splitOn (Strings.fromList [
                      13]) e2)
                e4 = Strings.intercalate "\\t" (Strings.splitOn (Strings.fromList [
                      9]) e3)
                escaped = Strings.intercalate "\\\"" (Strings.splitOn "\"" e4)
            in (Serialization.cst (Strings.cat [
              "\"",
              escaped,
              "\""]))
      Syntax.LiteralCharacter v0 ->
        let ch = Syntax.characterLiteralValue v0
        in case d of
          Syntax.DialectClojure -> Serialization.cst (Strings.cat2 "\\" ch)
          Syntax.DialectEmacsLisp -> Serialization.cst (Strings.cat2 "?" ch)
          Syntax.DialectCommonLisp -> Serialization.cst (Strings.cat2 "#\\" ch)
          Syntax.DialectScheme -> Serialization.cst (Strings.cat2 "#\\" ch)
      Syntax.LiteralBoolean v0 -> Logic.ifElse v0 (trueExpr d) (falseExpr d)
      Syntax.LiteralNil -> nilExpr d
      Syntax.LiteralKeyword v0 -> keywordToExpr d v0
      Syntax.LiteralSymbol v0 -> Serialization.noSep [
        Serialization.cst "'",
        (symbolToExpr v0)]

listLiteralToExpr :: Syntax.Dialect -> Syntax.ListLiteral -> Ast.Expr
listLiteralToExpr d ll =

      let elems = Lists.map (expressionToExpr d) (Syntax.listLiteralElements ll)
          quoted = Syntax.listLiteralQuoted ll
      in (Logic.ifElse quoted (Serialization.noSep [
        Serialization.cst "'",
        (Serialization.parens (Serialization.spaceSep elems))]) (Serialization.parens (Serialization.spaceSep (Lists.concat2 [
        Serialization.cst (listKeyword d)] elems))))

vectorLiteralToExpr :: Syntax.Dialect -> Syntax.VectorLiteral -> Ast.Expr
vectorLiteralToExpr d vl =

      let elems = Lists.map (expressionToExpr d) (Syntax.vectorLiteralElements vl)
      in case d of
        Syntax.DialectClojure -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep elems)
        Syntax.DialectEmacsLisp -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep elems)
        Syntax.DialectCommonLisp -> Serialization.noSep [
          Serialization.cst "#",
          (Serialization.parens (Serialization.spaceSep elems))]
        Syntax.DialectScheme -> Serialization.noSep [
          Serialization.cst "#",
          (Serialization.parens (Serialization.spaceSep elems))]

mapLiteralToExpr :: Syntax.Dialect -> Syntax.MapLiteral -> Ast.Expr
mapLiteralToExpr d ml =

      let entries = Syntax.mapLiteralEntries ml
      in case d of
        Syntax.DialectClojure -> Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (Serialization.spaceSep (Lists.concat (Lists.map (\e -> [
          expressionToExpr d (Syntax.mapEntryKey e),
          (expressionToExpr d (Syntax.mapEntryValue e))]) entries)))
        Syntax.DialectEmacsLisp -> Serialization.noSep [
          Serialization.cst "'",
          (Serialization.parens (Serialization.spaceSep (Lists.map (\e -> Serialization.parens (Serialization.spaceSep [
            expressionToExpr d (Syntax.mapEntryKey e),
            (Serialization.cst "."),
            (expressionToExpr d (Syntax.mapEntryValue e))])) entries)))]
        Syntax.DialectCommonLisp -> Serialization.noSep [
          Serialization.cst "'",
          (Serialization.parens (Serialization.spaceSep (Lists.map (\e -> Serialization.parens (Serialization.spaceSep [
            expressionToExpr d (Syntax.mapEntryKey e),
            (Serialization.cst "."),
            (expressionToExpr d (Syntax.mapEntryValue e))])) entries)))]
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst "list"] (Lists.map (\e -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "cons",
          (expressionToExpr d (Syntax.mapEntryKey e)),
          (expressionToExpr d (Syntax.mapEntryValue e))])) entries)))

setLiteralToExpr :: Syntax.Dialect -> Syntax.SetLiteral -> Ast.Expr
setLiteralToExpr d sl =

      let elems = Lists.map (expressionToExpr d) (Syntax.setLiteralElements sl)
      in case d of
        Syntax.DialectClojure -> Serialization.noSep [
          Serialization.cst "#",
          (Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (Serialization.spaceSep elems))]
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst "list"] elems))
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst "cl:list"] elems))
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst "list"] elems))

moduleDeclarationToExpr :: Syntax.Dialect -> Syntax.ModuleDeclaration -> Ast.Expr
moduleDeclarationToExpr d mdecl =

      let name = Syntax.unNamespaceName (Syntax.moduleDeclarationName mdecl)
      in case d of
        Syntax.DialectClojure -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "ns",
          (Serialization.cst name)])
        Syntax.DialectEmacsLisp -> Serialization.newlineSep [
          Serialization.parens (Serialization.spaceSep [
            Serialization.cst "require",
            (Serialization.noSep [
              Serialization.cst "'",
              (Serialization.cst "cl-lib")])]),
          (Serialization.parens (Serialization.spaceSep [
            Serialization.cst "provide",
            (Serialization.noSep [
              Serialization.cst "'",
              (Serialization.cst name)])]))]
        Syntax.DialectCommonLisp -> Serialization.newlineSep [
          Serialization.parens (Serialization.spaceSep [
            Serialization.cst "defpackage",
            (Serialization.cst (Strings.cat2 ":" name))]),
          (Serialization.parens (Serialization.spaceSep [
            Serialization.cst "in-package",
            (Serialization.cst (Strings.cat2 ":" name))]))]
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "define-library",
          (Serialization.parens (Serialization.cst name))])

importDeclarationToExpr :: Syntax.Dialect -> Syntax.ImportDeclaration -> Ast.Expr
importDeclarationToExpr d idecl =

      let modName = Syntax.unNamespaceName (Syntax.importDeclarationModule idecl)
      in case d of
        Syntax.DialectClojure -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst ":require",
          (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep [
            Serialization.cst modName]))])
        Syntax.DialectEmacsLisp -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "require",
          (Serialization.noSep [
            Serialization.cst "'",
            (Serialization.cst modName)])])
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst ":use",
          (Serialization.cst (Strings.cat2 ":" modName))])
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "import",
          (Serialization.parens (Serialization.cst modName))])

exportDeclarationToExpr :: Syntax.Dialect -> Syntax.ExportDeclaration -> Ast.Expr
exportDeclarationToExpr d edecl =

      let syms = Lists.map symbolToExpr (Syntax.exportDeclarationSymbols edecl)
      in case d of
        Syntax.DialectClojure -> Serialization.cst ""
        Syntax.DialectEmacsLisp -> Serialization.newlineSep (Lists.map (\s -> Serialization.parens (Serialization.spaceSep [
          Serialization.cst "provide",
          (Serialization.noSep [
            Serialization.cst "'",
            s])])) syms)
        Syntax.DialectCommonLisp -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst ":export"] (Lists.map (\s -> Serialization.noSep [
          Serialization.cst ":",
          s]) syms)))
        Syntax.DialectScheme -> Serialization.parens (Serialization.spaceSep (Lists.concat2 [
          Serialization.cst "export"] syms))

sExpressionToExpr :: Syntax.SExpression -> Ast.Expr
sExpressionToExpr sexpr =
    case sexpr of
      Syntax.SExpressionAtom v0 -> Serialization.cst v0
      Syntax.SExpressionList v0 -> Serialization.parens (Serialization.spaceSep (Lists.map sExpressionToExpr v0))

symbolToExpr :: Syntax.Symbol -> Ast.Expr
symbolToExpr s = Serialization.cst (Syntax.unSymbol s)

keywordToExpr :: Syntax.Dialect -> Syntax.Keyword -> Ast.Expr
keywordToExpr d k =

      let name = Syntax.keywordName k
          ns = Syntax.keywordNamespace k
      in case d of
        Syntax.DialectScheme -> Serialization.noSep [
          Serialization.cst "'",
          (Serialization.cst name)]
        _ -> Serialization.cst (Maybes.maybe (Strings.cat2 ":" name) (\n -> Strings.cat [
          n,
          "/:",
          name]) ns)

docstringToExpr :: Syntax.Docstring -> Ast.Expr
docstringToExpr ds =
    Serialization.cst (Strings.cat [
      ";; ",
      (Syntax.unDocstring ds)])

commentToExpr :: Syntax.Comment -> Ast.Expr
commentToExpr c =

      let text = Syntax.commentText c
      in (Serialization.cst (Strings.cat2 "; " text))
