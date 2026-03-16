-- Note: this is an automatically generated file. Do not edit.

-- | Haskell operator precendence and associativity are drawn from:
-- | https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- | Other operators were investigated using GHCi, e.g. ":info (->)"
-- | Operator names are drawn (loosely) from:
-- | https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Ext.Haskell.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Ext.Haskell.Ast as Ast_
import qualified Hydra.Ext.Haskell.Operators as Operators
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a pattern-matching alternative to an AST expression
alternativeToExpr :: Ast_.Alternative -> Ast.Expr
alternativeToExpr alt =
    Serialization.structuralSpaceSep [
      patternToExpr (Ast_.alternativePattern alt),
      (Serialization.cst "->"),
      (caseRhsToExpr (Ast_.alternativeRhs alt))]

-- | Convert a function application expression to an AST expression
applicationExpressionToExpr :: Ast_.ApplicationExpression -> Ast.Expr
applicationExpressionToExpr app =
    Serialization.ifx Operators.appOp (expressionToExpr (Ast_.applicationExpressionFunction app)) (expressionToExpr (Ast_.applicationExpressionArgument app))

-- | Convert an application pattern to an AST expression
applicationPatternToExpr :: Ast_.ApplicationPattern -> Ast.Expr
applicationPatternToExpr appPat =
     
      let name = Ast_.applicationPatternName appPat 
          pats = Ast_.applicationPatternArgs appPat
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.map patternToExpr pats)))

-- | Convert a type class assertion to an AST expression
assertionToExpr :: Ast_.Assertion -> Ast.Expr
assertionToExpr sert =
    case sert of
      Ast_.AssertionClass v0 -> classAssertionToExpr v0
      Ast_.AssertionTuple v0 -> Serialization.parenList False (Lists.map assertionToExpr v0)

-- | Convert a case expression to an AST expression
caseExpressionToExpr :: Ast_.CaseExpression -> Ast.Expr
caseExpressionToExpr caseExpr =
     
      let cs = Ast_.caseExpressionCase caseExpr 
          alts = Ast_.caseExpressionAlternatives caseExpr
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
caseRhsToExpr :: Ast_.CaseRhs -> Ast.Expr
caseRhsToExpr rhs = expressionToExpr (Ast_.unCaseRhs rhs)

-- | Convert a class assertion to an AST expression
classAssertionToExpr :: Ast_.ClassAssertion -> Ast.Expr
classAssertionToExpr clsAsrt =
     
      let name = Ast_.classAssertionName clsAsrt 
          types = Ast_.classAssertionTypes clsAsrt
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
        Serialization.commaSep Serialization.halfBlockStyle (Lists.map typeToExpr types)]))

-- | Convert a data constructor to an AST expression
constructorToExpr :: Ast_.Constructor -> Ast.Expr
constructorToExpr cons =
    case cons of
      Ast_.ConstructorOrdinary v0 ->  
        let name = Ast_.ordinaryConstructorName v0 
            types = Ast_.ordinaryConstructorFields v0
        in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
          Serialization.spaceSep (Lists.map typeToExpr types)]))
      Ast_.ConstructorRecord v0 ->  
        let name = Ast_.recordConstructorName v0 
            fields = Ast_.recordConstructorFields v0
        in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
          Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map fieldWithCommentsToExpr fields)]))

-- | Convert a data constructor with comments to an AST expression
constructorWithCommentsToExpr :: Ast_.ConstructorWithComments -> Ast.Expr
constructorWithCommentsToExpr consWithComments =
     
      let body = Ast_.constructorWithCommentsBody consWithComments 
          mc = Ast_.constructorWithCommentsComments consWithComments
      in (Maybes.maybe (constructorToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        constructorToExpr body])) mc)

-- | Convert a data/newtype keyword to an AST expression
dataOrNewtypeToExpr :: Ast_.DataOrNewtype -> Ast.Expr
dataOrNewtypeToExpr kw =
    case kw of
      Ast_.DataOrNewtypeData -> Serialization.cst "data"
      Ast_.DataOrNewtypeNewtype -> Serialization.cst "newtype"

-- | Convert a declaration head to an AST expression
declarationHeadToExpr :: Ast_.DeclarationHead -> Ast.Expr
declarationHeadToExpr hd =
    case hd of
      Ast_.DeclarationHeadApplication v0 ->  
        let fun = Ast_.applicationDeclarationHeadFunction v0 
            op = Ast_.applicationDeclarationHeadOperand v0
        in (Serialization.spaceSep (Lists.cons (declarationHeadToExpr fun) [
          variableToExpr op]))
      Ast_.DeclarationHeadSimple v0 -> nameToExpr v0

-- | Convert a declaration to an AST expression
declarationToExpr :: Ast_.Declaration -> Ast.Expr
declarationToExpr decl =
    case decl of
      Ast_.DeclarationData v0 ->  
        let kw = Ast_.dataDeclarationKeyword v0 
            hd = Ast_.dataDeclarationHead v0
            cons = Ast_.dataDeclarationConstructors v0
            deriv = Ast_.dataDeclarationDeriving v0
            derivCat = Lists.concat (Lists.map Ast_.unDeriving deriv)
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
      Ast_.DeclarationType v0 ->  
        let hd = Ast_.typeDeclarationName v0 
            typ = Ast_.typeDeclarationType v0
        in (Serialization.spaceSep (Lists.cons (Serialization.cst "type") (Lists.cons (declarationHeadToExpr hd) (Lists.cons (Serialization.cst "=") [
          typeToExpr typ]))))
      Ast_.DeclarationValueBinding v0 -> valueBindingToExpr v0
      Ast_.DeclarationTypedBinding v0 ->  
        let typeSig = Ast_.typedBindingTypeSignature v0 
            vb = Ast_.typedBindingValueBinding v0
            name = Ast_.typeSignatureName typeSig
            htype = Ast_.typeSignatureType typeSig
        in (Serialization.newlineSep (Lists.cons (Serialization.structuralSpaceSep [
          nameToExpr name,
          (Serialization.cst "::"),
          (typeToExpr htype)]) [
          valueBindingToExpr vb]))

-- | Convert a declaration with comments to an AST expression
declarationWithCommentsToExpr :: Ast_.DeclarationWithComments -> Ast.Expr
declarationWithCommentsToExpr declWithComments =
     
      let body = Ast_.declarationWithCommentsBody declWithComments 
          mc = Ast_.declarationWithCommentsComments declWithComments
      in (Maybes.maybe (declarationToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        declarationToExpr body])) mc)

-- | Convert a Haskell expression to an AST expression
expressionToExpr :: Ast_.Expression -> Ast.Expr
expressionToExpr expr =
    case expr of
      Ast_.ExpressionApplication v0 -> applicationExpressionToExpr v0
      Ast_.ExpressionCase v0 -> caseExpressionToExpr v0
      Ast_.ExpressionConstructRecord v0 -> constructRecordExpressionToExpr v0
      Ast_.ExpressionDo v0 -> Serialization.indentBlock (Lists.cons (Serialization.cst "do") (Lists.map statementToExpr v0))
      Ast_.ExpressionIf v0 -> ifExpressionToExpr v0
      Ast_.ExpressionLiteral v0 -> literalToExpr v0
      Ast_.ExpressionLambda v0 -> Serialization.parenthesize (lambdaExpressionToExpr v0)
      Ast_.ExpressionLet v0 ->  
        let bindings = Ast_.letExpressionBindings v0 
            inner = Ast_.letExpressionInner v0
            encodeBinding = \binding -> Serialization.indentSubsequentLines "    " (localBindingToExpr binding)
        in (Serialization.indentBlock (Lists.cons (Serialization.cst "") (Lists.cons (Serialization.spaceSep (Lists.cons (Serialization.cst "let") [
          Serialization.customIndentBlock "    " (Lists.map encodeBinding bindings)])) [
          Serialization.spaceSep (Lists.cons (Serialization.cst "in") [
            expressionToExpr inner])])))
      Ast_.ExpressionList v0 -> Serialization.bracketList Serialization.halfBlockStyle (Lists.map expressionToExpr v0)
      Ast_.ExpressionParens v0 -> Serialization.parenthesize (expressionToExpr v0)
      Ast_.ExpressionTuple v0 -> Serialization.parenList False (Lists.map expressionToExpr v0)
      Ast_.ExpressionVariable v0 -> nameToExpr v0

-- | Convert a record construction expression to an AST expression
constructRecordExpressionToExpr :: Ast_.ConstructRecordExpression -> Ast.Expr
constructRecordExpressionToExpr constructRecord =
     
      let name = Ast_.constructRecordExpressionName constructRecord 
          updates = Ast_.constructRecordExpressionFields constructRecord
          fromUpdate =
                  \update ->  
                    let fn = Ast_.fieldUpdateName update 
                        val = Ast_.fieldUpdateValue update
                    in (Serialization.ifx Operators.defineOp (nameToExpr fn) (expressionToExpr val))
          body = Serialization.commaSep Serialization.halfBlockStyle (Lists.map fromUpdate updates)
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
        Serialization.brackets Serialization.curlyBraces Serialization.halfBlockStyle body]))

-- | Convert a field declaration to an AST expression
fieldToExpr :: Ast_.Field -> Ast.Expr
fieldToExpr field =
     
      let name = Ast_.fieldName field 
          typ = Ast_.fieldType field
      in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.cons (Serialization.cst "::") [
        typeToExpr typ])))

-- | Convert a field with comments to an AST expression
fieldWithCommentsToExpr :: Ast_.FieldWithComments -> Ast.Expr
fieldWithCommentsToExpr fieldWithComments =
     
      let field = Ast_.fieldWithCommentsField fieldWithComments 
          mc = Ast_.fieldWithCommentsComments fieldWithComments
      in (Maybes.maybe (fieldToExpr field) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
        fieldToExpr field])) mc)

-- | Convert an if-then-else expression to an AST expression
ifExpressionToExpr :: Ast_.IfExpression -> Ast.Expr
ifExpressionToExpr ifExpr =
     
      let eif = Ast_.ifExpressionCondition ifExpr 
          ethen = Ast_.ifExpressionThen ifExpr
          eelse = Ast_.ifExpressionElse ifExpr
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
importExportSpecToExpr :: Ast_.ImportExportSpec -> Ast.Expr
importExportSpecToExpr spec = nameToExpr (Ast_.importExportSpecName spec)

-- | Convert an import statement to an AST expression
importToExpr :: Ast_.Import -> Ast.Expr
importToExpr import_ =
     
      let qual = Ast_.importQualified import_ 
          modName = Ast_.importModule import_
          mod = Ast_.importAs import_
          mspec = Ast_.importSpec import_
          name = Ast_.unModuleName modName
          hidingSec =
                  \spec -> case spec of
                    Ast_.SpecImportHiding v0 -> Serialization.spaceSep (Lists.cons (Serialization.cst "hiding ") [
                      Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map importExportSpecToExpr v0))])
          parts =
                  Maybes.cat [
                    Just (Serialization.cst "import"),
                    (Logic.ifElse qual (Just (Serialization.cst "qualified")) Nothing),
                    (Just (Serialization.cst name)),
                    (Maybes.map (\m -> Serialization.cst (Strings.cat2 "as " (Ast_.unModuleName m))) mod),
                    (Maybes.map hidingSec mspec)]
      in (Serialization.spaceSep parts)

-- | Convert a lambda expression to an AST expression
lambdaExpressionToExpr :: Ast_.LambdaExpression -> Ast.Expr
lambdaExpressionToExpr lambdaExpr =
     
      let bindings = Ast_.lambdaExpressionBindings lambdaExpr 
          inner = Ast_.lambdaExpressionInner lambdaExpr
          head = Serialization.spaceSep (Lists.map patternToExpr bindings)
          body = expressionToExpr inner
      in (Serialization.ifx Operators.lambdaOp (Serialization.prefix "\\" head) body)

-- | Convert a literal value to an AST expression
literalToExpr :: Ast_.Literal -> Ast.Expr
literalToExpr lit =
     
      let parensIfNeg =
              \b -> \e -> Logic.ifElse b (Strings.cat [
                "(",
                e,
                ")"]) e
      in (Serialization.cst (case lit of
        Ast_.LiteralChar v0 -> Literals.showString (Literals.showUint16 v0)
        Ast_.LiteralDouble v0 -> parensIfNeg (Equality.lt v0 0.0) (Literals.showFloat64 v0)
        Ast_.LiteralFloat v0 -> parensIfNeg (Equality.lt v0 0.0) (Literals.showFloat32 v0)
        Ast_.LiteralInt v0 -> parensIfNeg (Equality.lt v0 0) (Literals.showInt32 v0)
        Ast_.LiteralInteger v0 -> parensIfNeg (Equality.lt v0 0) (Literals.showBigint v0)
        Ast_.LiteralString v0 -> Literals.showString v0))

-- | Convert a local binding to an AST expression
localBindingToExpr :: Ast_.LocalBinding -> Ast.Expr
localBindingToExpr binding =
    case binding of
      Ast_.LocalBindingSignature v0 -> typeSignatureToExpr v0
      Ast_.LocalBindingValue v0 -> valueBindingToExpr v0

-- | Convert a module head to an AST expression
moduleHeadToExpr :: Ast_.ModuleHead -> Ast.Expr
moduleHeadToExpr moduleHead =
     
      let mc = Ast_.moduleHeadComments moduleHead 
          modName = Ast_.moduleHeadName moduleHead
          mname = Ast_.unModuleName modName
          head =
                  Serialization.spaceSep (Lists.cons (Serialization.cst "module") (Lists.cons (Serialization.cst mname) [
                    Serialization.cst "where"]))
      in (Maybes.maybe head (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) (Lists.cons (Serialization.cst "") [
        head]))) mc)

-- | Convert a Haskell module to an AST expression
moduleToExpr :: Ast_.Module -> Ast.Expr
moduleToExpr module_ =
     
      let mh = Ast_.moduleHead module_ 
          imports = Ast_.moduleImports module_
          decls = Ast_.moduleDeclarations module_
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
nameToExpr :: Ast_.Name -> Ast.Expr
nameToExpr name =
    Serialization.cst (case name of
      Ast_.NameImplicit v0 -> Strings.cat2 "?" (writeQualifiedName v0)
      Ast_.NameNormal v0 -> writeQualifiedName v0
      Ast_.NameParens v0 -> Strings.cat [
        "(",
        (writeQualifiedName v0),
        ")"])

-- | Convert a pattern to an AST expression
patternToExpr :: Ast_.Pattern -> Ast.Expr
patternToExpr pat =
    case pat of
      Ast_.PatternApplication v0 -> applicationPatternToExpr v0
      Ast_.PatternList v0 -> Serialization.bracketList Serialization.halfBlockStyle (Lists.map patternToExpr v0)
      Ast_.PatternLiteral v0 -> literalToExpr v0
      Ast_.PatternName v0 -> nameToExpr v0
      Ast_.PatternParens v0 -> Serialization.parenthesize (patternToExpr v0)
      Ast_.PatternTuple v0 -> Serialization.parenList False (Lists.map patternToExpr v0)
      Ast_.PatternWildcard -> Serialization.cst "_"

-- | Convert a right-hand side to an AST expression
rightHandSideToExpr :: Ast_.RightHandSide -> Ast.Expr
rightHandSideToExpr rhs = expressionToExpr (Ast_.unRightHandSide rhs)

-- | Convert a statement to an AST expression
statementToExpr :: Ast_.Statement -> Ast.Expr
statementToExpr stmt = expressionToExpr (Ast_.unStatement stmt)

-- | Convert a type signature to an AST expression
typeSignatureToExpr :: Ast_.TypeSignature -> Ast.Expr
typeSignatureToExpr typeSig =
     
      let name = Ast_.typeSignatureName typeSig 
          typ = Ast_.typeSignatureType typeSig
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
typeToExpr :: Ast_.Type -> Ast.Expr
typeToExpr htype =
    case htype of
      Ast_.TypeApplication v0 ->  
        let lhs = Ast_.applicationTypeContext v0 
            rhs = Ast_.applicationTypeArgument v0
        in (Serialization.ifx Operators.appOp (typeToExpr lhs) (typeToExpr rhs))
      Ast_.TypeCtx v0 ->  
        let ctx = Ast_.contextTypeCtx v0 
            typ = Ast_.contextTypeType v0
        in (Serialization.ifx Operators.assertOp (assertionToExpr ctx) (typeToExpr typ))
      Ast_.TypeFunction v0 ->  
        let dom = Ast_.functionTypeDomain v0 
            cod = Ast_.functionTypeCodomain v0
        in (Serialization.ifx Operators.arrowOp (typeToExpr dom) (typeToExpr cod))
      Ast_.TypeList v0 -> Serialization.bracketList Serialization.inlineStyle [
        typeToExpr v0]
      Ast_.TypeTuple v0 -> Serialization.parenList False (Lists.map typeToExpr v0)
      Ast_.TypeVariable v0 -> nameToExpr v0

-- | Convert a value binding to an AST expression
valueBindingToExpr :: Ast_.ValueBinding -> Ast.Expr
valueBindingToExpr vb =
    case vb of
      Ast_.ValueBindingSimple v0 ->  
        let pat = Ast_.simpleValueBindingPattern v0 
            rhs = Ast_.simpleValueBindingRhs v0
            local = Ast_.simpleValueBindingLocalBindings v0
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
          let bindings = Ast_.unLocalBindings localBindings
          in (Serialization.indentBlock (Lists.cons body [
            Serialization.indentBlock (Lists.cons (Serialization.cst "where") (Lists.map localBindingToExpr bindings))]))) local)

-- | Convert a type variable to an AST expression
variableToExpr :: Ast_.Variable -> Ast.Expr
variableToExpr variable = nameToExpr (Ast_.unVariable variable)

-- | Convert a string to Haddock documentation comments
toHaskellComments :: String -> String
toHaskellComments c = Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- | " s) (Strings.lines c))

-- | Convert a string to simple line comments
toSimpleComments :: String -> String
toSimpleComments c = Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- " s) (Strings.lines c))

-- | Write a qualified name as a string
writeQualifiedName :: Ast_.QualifiedName -> String
writeQualifiedName qname =
     
      let qualifiers = Ast_.qualifiedNameQualifiers qname 
          unqual = Ast_.qualifiedNameUnqualified qname
          h = \namePart -> Ast_.unNamePart namePart
          allParts = Lists.concat2 (Lists.map h qualifiers) [
                h unqual]
      in (Strings.intercalate "." allParts)
