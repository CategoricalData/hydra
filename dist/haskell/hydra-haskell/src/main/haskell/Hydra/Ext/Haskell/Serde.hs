-- Note: this is an automatically generated file. Do not edit.

-- | Haskell operator precendence and associativity are drawn from:
-- | https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- | Other operators were investigated using GHCi, e.g. ":info (->)"
-- | Operator names are drawn (loosely) from:
-- | https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Ext.Haskell.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Ext.Haskell.Operators as Operators
import qualified Hydra.Ext.Haskell.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Convert a pattern-matching alternative to an AST expression
alternativeToExpr :: Syntax.Alternative -> Ast.Expr
alternativeToExpr alt =
    Serialization.structuralSpaceSep [
      patternToExpr (Syntax.alternativePattern alt),
      (Serialization.cst "->"),
      (caseRhsToExpr (Syntax.alternativeRhs alt))]

-- | Convert a function application expression to an AST expression
applicationExpressionToExpr :: Syntax.ApplicationExpression -> Ast.Expr
applicationExpressionToExpr app =
    Serialization.ifx Operators.appOp (expressionToExpr (Syntax.applicationExpressionFunction app)) (expressionToExpr (Syntax.applicationExpressionArgument app))

-- | Convert an application pattern to an AST expression
applicationPatternToExpr :: Syntax.ApplicationPattern -> Ast.Expr
applicationPatternToExpr appPat =

      let name = Syntax.applicationPatternName appPat
          pats = Syntax.applicationPatternArgs appPat
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.map patternToExpr pats)))

-- | Convert a type class assertion to an AST expression
assertionToExpr :: Syntax.Assertion -> Ast.Expr
assertionToExpr sert =
    case sert of
      Syntax.AssertionClass v0 -> classAssertionToExpr v0
      Syntax.AssertionTuple v0 -> Serialization.parenList False (Lists.map assertionToExpr v0)

-- | Convert a case expression to an AST expression
caseExpressionToExpr :: Syntax.CaseExpression -> Ast.Expr
caseExpressionToExpr caseExpr =

      let cs = Syntax.caseExpressionCase caseExpr
          alts = Syntax.caseExpressionAlternatives caseExpr
          ofOp =
                  Ast.Op {
                    Ast.opSymbol = (Ast.Symbol "of"),
                    Ast.opPadding = Ast.Padding {
                      Ast.paddingLeft = Ast.WsSpace,
                      Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
                    Ast.opPrecedence = (Ast.Precedence 0),
                    Ast.opAssociativity = Ast.AssociativityNone}
          lhs =
                  Serialization.spaceSep [
                    Serialization.cst "case",
                    (expressionToExpr cs)]
          rhs = Serialization.newlineSep (Lists.map alternativeToExpr alts)
      in (Serialization.ifx ofOp lhs rhs)

-- | Convert a case right-hand side to an AST expression
caseRhsToExpr :: Syntax.CaseRhs -> Ast.Expr
caseRhsToExpr rhs = expressionToExpr (Syntax.unCaseRhs rhs)

-- | Convert a class assertion to an AST expression
classAssertionToExpr :: Syntax.ClassAssertion -> Ast.Expr
classAssertionToExpr clsAsrt =

      let name = Syntax.classAssertionName clsAsrt
          types = Syntax.classAssertionTypes clsAsrt
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
        Serialization.commaSep Serialization.halfBlockStyle (Lists.map typeToExpr types)]))

-- | Convert a record construction expression to an AST expression
constructRecordExpressionToExpr :: Syntax.ConstructRecordExpression -> Ast.Expr
constructRecordExpressionToExpr constructRecord =

      let name = Syntax.constructRecordExpressionName constructRecord
          updates = Syntax.constructRecordExpressionFields constructRecord
          fromUpdate =
                  \update ->
                    let fn = Syntax.fieldUpdateName update
                        val = Syntax.fieldUpdateValue update
                    in (Serialization.ifx Operators.defineOp (nameToExpr fn) (expressionToExpr val))
          body = Serialization.commaSep Serialization.halfBlockStyle (Lists.map fromUpdate updates)
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
        Serialization.brackets Serialization.curlyBraces Serialization.halfBlockStyle body]))

-- | Convert a data constructor to an AST expression
constructorToExpr :: Syntax.Constructor -> Ast.Expr
constructorToExpr cons =
    case cons of
      Syntax.ConstructorOrdinary v0 ->
        let name = Syntax.ordinaryConstructorName v0
            types = Syntax.ordinaryConstructorFields v0
        in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
          Serialization.spaceSep (Lists.map typeToExpr types)]))
      Syntax.ConstructorRecord v0 ->
        let name = Syntax.recordConstructorName v0
            fields = Syntax.recordConstructorFields v0
        in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
          Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map fieldWithCommentsToExpr fields)]))

-- | Convert a data constructor with comments to an AST expression
constructorWithCommentsToExpr :: Syntax.ConstructorWithComments -> Ast.Expr
constructorWithCommentsToExpr consWithComments =

      let body = Syntax.constructorWithCommentsBody consWithComments
          mc = Syntax.constructorWithCommentsComments consWithComments
      in (Maybes.maybe (constructorToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        constructorToExpr body])) mc)

-- | Convert a data/newtype keyword to an AST expression
dataOrNewtypeToExpr :: Syntax.DataOrNewtype -> Ast.Expr
dataOrNewtypeToExpr kw =
    case kw of
      Syntax.DataOrNewtypeData -> Serialization.cst "data"
      Syntax.DataOrNewtypeNewtype -> Serialization.cst "newtype"

-- | Convert a declaration head to an AST expression
declarationHeadToExpr :: Syntax.DeclarationHead -> Ast.Expr
declarationHeadToExpr hd =
    case hd of
      Syntax.DeclarationHeadApplication v0 ->
        let fun = Syntax.applicationDeclarationHeadFunction v0
            op = Syntax.applicationDeclarationHeadOperand v0
        in (Serialization.spaceSep (Lists.cons (declarationHeadToExpr fun) [
          variableToExpr op]))
      Syntax.DeclarationHeadSimple v0 -> nameToExpr v0

-- | Convert a declaration to an AST expression
declarationToExpr :: Syntax.Declaration -> Ast.Expr
declarationToExpr decl =
    case decl of
      Syntax.DeclarationData v0 ->
        let kw = Syntax.dataDeclarationKeyword v0
            hd = Syntax.dataDeclarationHead v0
            cons = Syntax.dataDeclarationConstructors v0
            deriv = Syntax.dataDeclarationDeriving v0
            derivCat = Lists.concat (Lists.map Syntax.unDeriving deriv)
            constructors = Serialization.orSep Serialization.halfBlockStyle (Lists.map constructorWithCommentsToExpr cons)
            derivingClause =
                    Logic.ifElse (Lists.null derivCat) [] [
                      Serialization.spaceSep (Lists.cons (Serialization.cst "deriving") [
                        Serialization.parenList False (Lists.map nameToExpr derivCat)])]
            mainParts =
                    [
                      Serialization.spaceSep (Lists.cons (dataOrNewtypeToExpr kw) (Lists.cons (declarationHeadToExpr hd) [
                        Serialization.cst "="])),
                      constructors]
        in (Serialization.indentBlock (Lists.concat2 mainParts derivingClause))
      Syntax.DeclarationType v0 ->
        let hd = Syntax.typeDeclarationName v0
            typ = Syntax.typeDeclarationType v0
        in (Serialization.spaceSep (Lists.cons (Serialization.cst "type") (Lists.cons (declarationHeadToExpr hd) (Lists.cons (Serialization.cst "=") [
          typeToExpr typ]))))
      Syntax.DeclarationValueBinding v0 -> valueBindingToExpr v0
      Syntax.DeclarationTypedBinding v0 ->
        let typeSig = Syntax.typedBindingTypeSignature v0
            vb = Syntax.typedBindingValueBinding v0
            name = Syntax.typeSignatureName typeSig
            htype = Syntax.typeSignatureType typeSig
        in (Serialization.newlineSep (Lists.cons (Serialization.structuralSpaceSep [
          nameToExpr name,
          (Serialization.cst "::"),
          (typeToExpr htype)]) [
          valueBindingToExpr vb]))

-- | Convert a declaration with comments to an AST expression
declarationWithCommentsToExpr :: Syntax.DeclarationWithComments -> Ast.Expr
declarationWithCommentsToExpr declWithComments =

      let body = Syntax.declarationWithCommentsBody declWithComments
          mc = Syntax.declarationWithCommentsComments declWithComments
      in (Maybes.maybe (declarationToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        declarationToExpr body])) mc)

-- | Convert a Haskell expression to an AST expression
expressionToExpr :: Syntax.Expression -> Ast.Expr
expressionToExpr expr =
    case expr of
      Syntax.ExpressionApplication v0 -> applicationExpressionToExpr v0
      Syntax.ExpressionCase v0 -> caseExpressionToExpr v0
      Syntax.ExpressionConstructRecord v0 -> constructRecordExpressionToExpr v0
      Syntax.ExpressionDo v0 -> Serialization.indentBlock (Lists.cons (Serialization.cst "do") (Lists.map statementToExpr v0))
      Syntax.ExpressionIf v0 -> ifExpressionToExpr v0
      Syntax.ExpressionLiteral v0 -> literalToExpr v0
      Syntax.ExpressionLambda v0 -> Serialization.parenthesize (lambdaExpressionToExpr v0)
      Syntax.ExpressionLet v0 ->
        let bindings = Syntax.letExpressionBindings v0
            inner = Syntax.letExpressionInner v0
            encodeBinding = \binding -> Serialization.indentSubsequentLines "    " (localBindingToExpr binding)
        in (Serialization.indentBlock (Lists.cons (Serialization.cst "") (Lists.cons (Serialization.spaceSep (Lists.cons (Serialization.cst "let") [
          Serialization.customIndentBlock "    " (Lists.map encodeBinding bindings)])) [
          Serialization.spaceSep (Lists.cons (Serialization.cst "in") [
            expressionToExpr inner])])))
      Syntax.ExpressionList v0 -> Serialization.bracketList Serialization.halfBlockStyle (Lists.map expressionToExpr v0)
      Syntax.ExpressionParens v0 -> Serialization.parenthesize (expressionToExpr v0)
      Syntax.ExpressionTuple v0 -> Serialization.parenList False (Lists.map expressionToExpr v0)
      Syntax.ExpressionVariable v0 -> nameToExpr v0

-- | Convert a field declaration to an AST expression
fieldToExpr :: Syntax.Field -> Ast.Expr
fieldToExpr field =

      let name = Syntax.fieldName field
          typ = Syntax.fieldType field
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.cons (Serialization.cst "::") [
        typeToExpr typ])))

-- | Convert a field with comments to an AST expression
fieldWithCommentsToExpr :: Syntax.FieldWithComments -> Ast.Expr
fieldWithCommentsToExpr fieldWithComments =

      let field = Syntax.fieldWithCommentsField fieldWithComments
          mc = Syntax.fieldWithCommentsComments fieldWithComments
      in (Maybes.maybe (fieldToExpr field) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        fieldToExpr field])) mc)

-- | Convert an if-then-else expression to an AST expression
ifExpressionToExpr :: Syntax.IfExpression -> Ast.Expr
ifExpressionToExpr ifExpr =

      let eif = Syntax.ifExpressionCondition ifExpr
          ethen = Syntax.ifExpressionThen ifExpr
          eelse = Syntax.ifExpressionElse ifExpr
          ifOp =
                  Ast.Op {
                    Ast.opSymbol = (Ast.Symbol ""),
                    Ast.opPadding = Ast.Padding {
                      Ast.paddingLeft = Ast.WsNone,
                      Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
                    Ast.opPrecedence = (Ast.Precedence 0),
                    Ast.opAssociativity = Ast.AssociativityNone}
          body =
                  Serialization.newlineSep (Lists.cons (Serialization.spaceSep (Lists.cons (Serialization.cst "then") [
                    expressionToExpr ethen])) [
                    Serialization.spaceSep (Lists.cons (Serialization.cst "else") [
                      expressionToExpr eelse])])
      in (Serialization.ifx ifOp (Serialization.spaceSep (Lists.cons (Serialization.cst "if") [
        expressionToExpr eif])) body)

-- | Convert an import/export specification to an AST expression
importExportSpecToExpr :: Syntax.ImportExportSpec -> Ast.Expr
importExportSpecToExpr spec = nameToExpr (Syntax.importExportSpecName spec)

-- | Convert an import statement to an AST expression
importToExpr :: Syntax.Import -> Ast.Expr
importToExpr import_ =

      let qual = Syntax.importQualified import_
          modName = Syntax.importModule import_
          mod = Syntax.importAs import_
          mspec = Syntax.importSpec import_
          name = Syntax.unModuleName modName
          hidingSec =
                  \spec -> case spec of
                    Syntax.SpecImportHiding v0 -> Serialization.spaceSep (Lists.cons (Serialization.cst "hiding ") [
                      Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map importExportSpecToExpr v0))])
          parts =
                  Maybes.cat [
                    Just (Serialization.cst "import"),
                    (Logic.ifElse qual (Just (Serialization.cst "qualified")) Nothing),
                    (Just (Serialization.cst name)),
                    (Maybes.map (\m -> Serialization.cst (Strings.cat2 "as " (Syntax.unModuleName m))) mod),
                    (Maybes.map hidingSec mspec)]
      in (Serialization.spaceSep parts)

-- | Convert a lambda expression to an AST expression
lambdaExpressionToExpr :: Syntax.LambdaExpression -> Ast.Expr
lambdaExpressionToExpr lambdaExpr =

      let bindings = Syntax.lambdaExpressionBindings lambdaExpr
          inner = Syntax.lambdaExpressionInner lambdaExpr
          head = Serialization.spaceSep (Lists.map patternToExpr bindings)
          body = expressionToExpr inner
      in (Serialization.ifx Operators.lambdaOp (Serialization.prefix "\\" head) body)

-- | Convert a literal value to an AST expression
literalToExpr :: Syntax.Literal -> Ast.Expr
literalToExpr lit =

      let parensIfNeg =
              \b -> \e -> Logic.ifElse b (Strings.cat [
                "(",
                e,
                ")"]) e
          showFloat =
                  \showFn -> \v ->
                    let raw = showFn v
                    in (Logic.ifElse (Equality.equal raw "NaN") "(0/0)" (Logic.ifElse (Equality.equal raw "Infinity") "(1/0)" (Logic.ifElse (Equality.equal raw "-Infinity") "(-(1/0))" (parensIfNeg (Equality.equal (Strings.charAt 0 raw) 45) raw))))
      in (Serialization.cst (case lit of
        Syntax.LiteralChar v0 -> Literals.showString (Literals.showUint16 v0)
        Syntax.LiteralDouble v0 -> showFloat (\v -> Literals.showFloat64 v) v0
        Syntax.LiteralFloat v0 -> showFloat (\v -> Literals.showFloat32 v) v0
        Syntax.LiteralInt v0 -> parensIfNeg (Equality.lt v0 0) (Literals.showInt32 v0)
        Syntax.LiteralInteger v0 -> parensIfNeg (Equality.lt v0 0) (Literals.showBigint v0)
        Syntax.LiteralString v0 -> Literals.showString v0))

-- | Convert a local binding to an AST expression
localBindingToExpr :: Syntax.LocalBinding -> Ast.Expr
localBindingToExpr binding =
    case binding of
      Syntax.LocalBindingSignature v0 -> typeSignatureToExpr v0
      Syntax.LocalBindingValue v0 -> valueBindingToExpr v0

-- | Convert a module head to an AST expression
moduleHeadToExpr :: Syntax.ModuleHead -> Ast.Expr
moduleHeadToExpr moduleHead =

      let mc = Syntax.moduleHeadComments moduleHead
          modName = Syntax.moduleHeadName moduleHead
          mname = Syntax.unModuleName modName
          head =
                  Serialization.spaceSep (Lists.cons (Serialization.cst "module") (Lists.cons (Serialization.cst mname) [
                    Serialization.cst "where"]))
      in (Maybes.maybe head (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) (Lists.cons (Serialization.cst "") [
        head]))) mc)

-- | Convert a Haskell module to an AST expression
moduleToExpr :: Syntax.Module -> Ast.Expr
moduleToExpr module_ =

      let mh = Syntax.moduleHead module_
          imports = Syntax.moduleImports module_
          decls = Syntax.moduleDeclarations module_
          warning = [
                Serialization.cst (toSimpleComments Constants.warningAutoGeneratedFile)]
          headerLine = Maybes.maybe [] (\h -> [
                moduleHeadToExpr h]) mh
          declLines = Lists.map declarationWithCommentsToExpr decls
          importLines = Logic.ifElse (Lists.null imports) [] [
                Serialization.newlineSep (Lists.map importToExpr imports)]
      in (Serialization.doubleNewlineSep (Lists.concat [
        warning,
        headerLine,
        importLines,
        declLines]))

-- | Convert a Haskell name to an AST expression
nameToExpr :: Syntax.Name -> Ast.Expr
nameToExpr name =
    Serialization.cst (case name of
      Syntax.NameImplicit v0 -> Strings.cat2 "?" (writeQualifiedName v0)
      Syntax.NameNormal v0 -> writeQualifiedName v0
      Syntax.NameParens v0 -> Strings.cat [
        "(",
        (writeQualifiedName v0),
        ")"])

-- | Convert a pattern to an AST expression
patternToExpr :: Syntax.Pattern -> Ast.Expr
patternToExpr pat =
    case pat of
      Syntax.PatternApplication v0 -> applicationPatternToExpr v0
      Syntax.PatternList v0 -> Serialization.bracketList Serialization.halfBlockStyle (Lists.map patternToExpr v0)
      Syntax.PatternLiteral v0 -> literalToExpr v0
      Syntax.PatternName v0 -> nameToExpr v0
      Syntax.PatternParens v0 -> Serialization.parenthesize (patternToExpr v0)
      Syntax.PatternTuple v0 -> Serialization.parenList False (Lists.map patternToExpr v0)
      Syntax.PatternWildcard -> Serialization.cst "_"

-- | Convert a right-hand side to an AST expression
rightHandSideToExpr :: Syntax.RightHandSide -> Ast.Expr
rightHandSideToExpr rhs = expressionToExpr (Syntax.unRightHandSide rhs)

-- | Convert a statement to an AST expression
statementToExpr :: Syntax.Statement -> Ast.Expr
statementToExpr stmt = expressionToExpr (Syntax.unStatement stmt)

-- | Convert a string to Haddock documentation comments
toHaskellComments :: String -> String
toHaskellComments c = Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- | " s) (Strings.lines c))

-- | Convert a string to simple line comments
toSimpleComments :: String -> String
toSimpleComments c = Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- " s) (Strings.lines c))

-- | Convert a type signature to an AST expression
typeSignatureToExpr :: Syntax.TypeSignature -> Ast.Expr
typeSignatureToExpr typeSig =

      let name = Syntax.typeSignatureName typeSig
          typ = Syntax.typeSignatureType typeSig
          nameExpr = nameToExpr name
          typeExpr = typeToExpr typ
          inlineSig =
                  Serialization.structuralSpaceSep [
                    nameExpr,
                    (Serialization.cst "::"),
                    typeExpr]
      in (Logic.ifElse (Equality.gt (Serialization.expressionLength inlineSig) 120) (Serialization.newlineSep [
        Serialization.spaceSep [
          nameExpr,
          (Serialization.cst "::")],
        (Serialization.tabIndent typeExpr)]) inlineSig)

-- | Convert a Haskell type to an AST expression
typeToExpr :: Syntax.Type -> Ast.Expr
typeToExpr htype =
    case htype of
      Syntax.TypeApplication v0 ->
        let lhs = Syntax.applicationTypeContext v0
            rhs = Syntax.applicationTypeArgument v0
        in (Serialization.ifx Operators.appOp (typeToExpr lhs) (typeToExpr rhs))
      Syntax.TypeCtx v0 ->
        let ctx = Syntax.contextTypeCtx v0
            typ = Syntax.contextTypeType v0
        in (Serialization.ifx Operators.assertOp (assertionToExpr ctx) (typeToExpr typ))
      Syntax.TypeFunction v0 ->
        let dom = Syntax.functionTypeDomain v0
            cod = Syntax.functionTypeCodomain v0
        in (Serialization.ifx Operators.arrowOp (typeToExpr dom) (typeToExpr cod))
      Syntax.TypeList v0 -> Serialization.bracketList Serialization.inlineStyle [
        typeToExpr v0]
      Syntax.TypeTuple v0 -> Serialization.parenList False (Lists.map typeToExpr v0)
      Syntax.TypeVariable v0 -> nameToExpr v0

-- | Convert a value binding to an AST expression
valueBindingToExpr :: Syntax.ValueBinding -> Ast.Expr
valueBindingToExpr vb =
    case vb of
      Syntax.ValueBindingSimple v0 ->
        let pat = Syntax.simpleValueBindingPattern v0
            rhs = Syntax.simpleValueBindingRhs v0
            local = Syntax.simpleValueBindingLocalBindings v0
            lhsExpr = patternToExpr pat
            rhsExpr = rightHandSideToExpr rhs
            inlineBody =
                    Serialization.structuralSpaceSep [
                      lhsExpr,
                      (Serialization.cst "="),
                      rhsExpr]
            body =
                    Logic.ifElse (Equality.gt (Serialization.expressionLength inlineBody) 120) (Serialization.newlineSep [
                      Serialization.spaceSep [
                        lhsExpr,
                        (Serialization.cst "=")],
                      (Serialization.tabIndent rhsExpr)]) inlineBody
        in (Maybes.maybe body (\localBindings ->
          let bindings = Syntax.unLocalBindings localBindings
          in (Serialization.indentBlock (Lists.cons body [
            Serialization.indentBlock (Lists.cons (Serialization.cst "where") (Lists.map localBindingToExpr bindings))]))) local)

-- | Convert a type variable to an AST expression
variableToExpr :: Syntax.Variable -> Ast.Expr
variableToExpr variable = nameToExpr (Syntax.unVariable variable)

-- | Write a qualified name as a string
writeQualifiedName :: Syntax.QualifiedName -> String
writeQualifiedName qname =

      let qualifiers = Syntax.qualifiedNameQualifiers qname
          unqual = Syntax.qualifiedNameUnqualified qname
          h = \namePart -> Syntax.unNamePart namePart
          allParts = Lists.concat2 (Lists.map h qualifiers) [
                h unqual]
      in (Strings.intercalate "." allParts)
