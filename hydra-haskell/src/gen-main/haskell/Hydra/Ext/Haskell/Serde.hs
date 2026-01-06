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

alternativeToExpr :: (Ast_.Alternative -> Ast.Expr)
alternativeToExpr alt = (Serialization.ifx Operators.caseOp (patternToExpr (Ast_.alternativePattern alt)) (caseRhsToExpr (Ast_.alternativeRhs alt)))

applicationExpressionToExpr :: (Ast_.ApplicationExpression -> Ast.Expr)
applicationExpressionToExpr app = (Serialization.ifx Operators.appOp (expressionToExpr (Ast_.applicationExpressionFunction app)) (expressionToExpr (Ast_.applicationExpressionArgument app)))

applicationPatternToExpr :: (Ast_.ApplicationPattern -> Ast.Expr)
applicationPatternToExpr appPat =  
  let name = (Ast_.applicationPatternName appPat) 
      pats = (Ast_.applicationPatternArgs appPat)
  in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.map patternToExpr pats)))

assertionToExpr :: (Ast_.Assertion -> Ast.Expr)
assertionToExpr sert = ((\x -> case x of
  Ast_.AssertionClass v1 -> (classAssertionToExpr v1)
  Ast_.AssertionTuple v1 -> (Serialization.parenList False (Lists.map assertionToExpr v1))) sert)

caseExpressionToExpr :: (Ast_.CaseExpression -> Ast.Expr)
caseExpressionToExpr caseExpr =  
  let cs = (Ast_.caseExpressionCase caseExpr) 
      alts = (Ast_.caseExpressionAlternatives caseExpr)
      ofOp = Ast.Op {
              Ast.opSymbol = (Ast.Symbol "of"),
              Ast.opPadding = Ast.Padding {
                Ast.paddingLeft = Ast.WsSpace,
                Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
              Ast.opPrecedence = (Ast.Precedence 0),
              Ast.opAssociativity = Ast.AssociativityNone}
      lhs = (Serialization.spaceSep [
              Serialization.cst "case",
              (expressionToExpr cs)])
      rhs = (Serialization.newlineSep (Lists.map alternativeToExpr alts))
  in (Serialization.ifx ofOp lhs rhs)

caseRhsToExpr :: (Ast_.CaseRhs -> Ast.Expr)
caseRhsToExpr rhs = (expressionToExpr (Ast_.unCaseRhs rhs))

classAssertionToExpr :: (Ast_.ClassAssertion -> Ast.Expr)
classAssertionToExpr clsAsrt =  
  let name = (Ast_.classAssertionName clsAsrt) 
      types = (Ast_.classAssertionTypes clsAsrt)
  in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
    Serialization.commaSep Serialization.halfBlockStyle (Lists.map typeToExpr types)]))

constructorToExpr :: (Ast_.Constructor -> Ast.Expr)
constructorToExpr cons = ((\x -> case x of
  Ast_.ConstructorOrdinary v1 ->  
    let name = (Ast_.ordinaryConstructorName v1) 
        types = (Ast_.ordinaryConstructorFields v1)
    in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
      Serialization.spaceSep (Lists.map typeToExpr types)]))
  Ast_.ConstructorRecord v1 ->  
    let name = (Ast_.recordConstructorName v1) 
        fields = (Ast_.recordConstructorFields v1)
    in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
      Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map fieldWithCommentsToExpr fields)]))) cons)

constructorWithCommentsToExpr :: (Ast_.ConstructorWithComments -> Ast.Expr)
constructorWithCommentsToExpr consWithComments =  
  let body = (Ast_.constructorWithCommentsBody consWithComments) 
      mc = (Ast_.constructorWithCommentsComments consWithComments)
  in (Maybes.maybe (constructorToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
    constructorToExpr body])) mc)

dataOrNewtypeToExpr :: (Ast_.DataOrNewtype -> Ast.Expr)
dataOrNewtypeToExpr kw = ((\x -> case x of
  Ast_.DataOrNewtypeData -> (Serialization.cst "data")
  Ast_.DataOrNewtypeNewtype -> (Serialization.cst "newtype")) kw)

declarationHeadToExpr :: (Ast_.DeclarationHead -> Ast.Expr)
declarationHeadToExpr hd = ((\x -> case x of
  Ast_.DeclarationHeadApplication v1 ->  
    let fun = (Ast_.applicationDeclarationHeadFunction v1) 
        op = (Ast_.applicationDeclarationHeadOperand v1)
    in (Serialization.spaceSep (Lists.cons (declarationHeadToExpr fun) [
      variableToExpr op]))
  Ast_.DeclarationHeadSimple v1 -> (nameToExpr v1)) hd)

declarationToExpr :: (Ast_.Declaration -> Ast.Expr)
declarationToExpr decl = ((\x -> case x of
  Ast_.DeclarationData v1 ->  
    let kw = (Ast_.dataDeclarationKeyword v1) 
        hd = (Ast_.dataDeclarationHead v1)
        cons = (Ast_.dataDeclarationConstructors v1)
        deriv = (Ast_.dataDeclarationDeriving v1)
        derivCat = (Lists.concat (Lists.map Ast_.unDeriving deriv))
        constructors = (Serialization.orSep Serialization.halfBlockStyle (Lists.map constructorWithCommentsToExpr cons))
        derivingClause = (Logic.ifElse (Lists.null derivCat) [] [
                Serialization.spaceSep (Lists.cons (Serialization.cst "deriving") [
                  Serialization.parenList False (Lists.map nameToExpr derivCat)])])
        mainParts = [
                Serialization.spaceSep (Lists.cons (dataOrNewtypeToExpr kw) (Lists.cons (declarationHeadToExpr hd) [
                  Serialization.cst "="])),
                constructors]
    in (Serialization.indentBlock (Lists.concat2 mainParts derivingClause))
  Ast_.DeclarationType v1 ->  
    let hd = (Ast_.typeDeclarationName v1) 
        typ = (Ast_.typeDeclarationType v1)
    in (Serialization.spaceSep (Lists.cons (Serialization.cst "type") (Lists.cons (declarationHeadToExpr hd) (Lists.cons (Serialization.cst "=") [
      typeToExpr typ]))))
  Ast_.DeclarationValueBinding v1 -> (valueBindingToExpr v1)
  Ast_.DeclarationTypedBinding v1 ->  
    let typeSig = (Ast_.typedBindingTypeSignature v1) 
        vb = (Ast_.typedBindingValueBinding v1)
        name = (Ast_.typeSignatureName typeSig)
        htype = (Ast_.typeSignatureType typeSig)
    in (Serialization.newlineSep (Lists.cons (Serialization.ifx Operators.typeOp (nameToExpr name) (typeToExpr htype)) [
      valueBindingToExpr vb]))) decl)

declarationWithCommentsToExpr :: (Ast_.DeclarationWithComments -> Ast.Expr)
declarationWithCommentsToExpr declWithComments =  
  let body = (Ast_.declarationWithCommentsBody declWithComments) 
      mc = (Ast_.declarationWithCommentsComments declWithComments)
  in (Maybes.maybe (declarationToExpr body) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
    declarationToExpr body])) mc)

expressionToExpr :: (Ast_.Expression -> Ast.Expr)
expressionToExpr expr = ((\x -> case x of
  Ast_.ExpressionApplication v1 -> (applicationExpressionToExpr v1)
  Ast_.ExpressionCase v1 -> (caseExpressionToExpr v1)
  Ast_.ExpressionConstructRecord v1 -> (constructRecordExpressionToExpr v1)
  Ast_.ExpressionDo v1 -> (Serialization.indentBlock (Lists.cons (Serialization.cst "do") (Lists.map statementToExpr v1)))
  Ast_.ExpressionIf v1 -> (ifExpressionToExpr v1)
  Ast_.ExpressionLiteral v1 -> (literalToExpr v1)
  Ast_.ExpressionLambda v1 -> (Serialization.parenthesize (lambdaExpressionToExpr v1))
  Ast_.ExpressionLet v1 ->  
    let bindings = (Ast_.letExpressionBindings v1) 
        inner = (Ast_.letExpressionInner v1)
        encodeBinding = (\binding -> Serialization.indentSubsequentLines "      " (localBindingToExpr binding))
    in (Serialization.indentBlock (Lists.cons (Serialization.cst "") (Lists.cons (Serialization.spaceSep (Lists.cons (Serialization.cst "let") [
      Serialization.customIndentBlock "    " (Lists.map encodeBinding bindings)])) [
      Serialization.spaceSep (Lists.cons (Serialization.cst "in") [
        expressionToExpr inner])])))
  Ast_.ExpressionList v1 -> (Serialization.bracketList Serialization.halfBlockStyle (Lists.map expressionToExpr v1))
  Ast_.ExpressionParens v1 -> (Serialization.parenthesize (expressionToExpr v1))
  Ast_.ExpressionTuple v1 -> (Serialization.parenList False (Lists.map expressionToExpr v1))
  Ast_.ExpressionVariable v1 -> (nameToExpr v1)) expr)

constructRecordExpressionToExpr :: (Ast_.ConstructRecordExpression -> Ast.Expr)
constructRecordExpressionToExpr constructRecord =  
  let name = (Ast_.constructRecordExpressionName constructRecord) 
      updates = (Ast_.constructRecordExpressionFields constructRecord)
      fromUpdate = (\update ->  
              let fn = (Ast_.fieldUpdateName update) 
                  val = (Ast_.fieldUpdateValue update)
              in (Serialization.ifx Operators.defineOp (nameToExpr fn) (expressionToExpr val)))
      body = (Serialization.commaSep Serialization.halfBlockStyle (Lists.map fromUpdate updates))
  in (Serialization.spaceSep (Lists.cons (nameToExpr name) [
    Serialization.brackets Serialization.curlyBraces Serialization.halfBlockStyle body]))

fieldToExpr :: (Ast_.Field -> Ast.Expr)
fieldToExpr field =  
  let name = (Ast_.fieldName field) 
      typ = (Ast_.fieldType field)
  in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.cons (Serialization.cst "::") [
    typeToExpr typ])))

fieldWithCommentsToExpr :: (Ast_.FieldWithComments -> Ast.Expr)
fieldWithCommentsToExpr fieldWithComments =  
  let field = (Ast_.fieldWithCommentsField fieldWithComments) 
      mc = (Ast_.fieldWithCommentsComments fieldWithComments)
  in (Maybes.maybe (fieldToExpr field) (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) [
    fieldToExpr field])) mc)

ifExpressionToExpr :: (Ast_.IfExpression -> Ast.Expr)
ifExpressionToExpr ifExpr =  
  let eif = (Ast_.ifExpressionCondition ifExpr) 
      ethen = (Ast_.ifExpressionThen ifExpr)
      eelse = (Ast_.ifExpressionElse ifExpr)
      ifOp = Ast.Op {
              Ast.opSymbol = (Ast.Symbol ""),
              Ast.opPadding = Ast.Padding {
                Ast.paddingLeft = Ast.WsNone,
                Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
              Ast.opPrecedence = (Ast.Precedence 0),
              Ast.opAssociativity = Ast.AssociativityNone}
      body = (Serialization.newlineSep (Lists.cons (Serialization.spaceSep (Lists.cons (Serialization.cst "then") [
              expressionToExpr ethen])) [
              Serialization.spaceSep (Lists.cons (Serialization.cst "else") [
                expressionToExpr eelse])]))
  in (Serialization.ifx ifOp (Serialization.spaceSep (Lists.cons (Serialization.cst "if") [
    expressionToExpr eif])) body)

importExportSpecToExpr :: (Ast_.ImportExportSpec -> Ast.Expr)
importExportSpecToExpr spec = (nameToExpr (Ast_.importExportSpecName spec))

importToExpr :: (Ast_.Import -> Ast.Expr)
importToExpr import_ =  
  let qual = (Ast_.importQualified import_) 
      modName = (Ast_.importModule import_)
      mod = (Ast_.importAs import_)
      mspec = (Ast_.importSpec import_)
      name = (Ast_.unModuleName modName)
      hidingSec = (\spec -> (\x -> case x of
              Ast_.SpecImportHiding v1 -> (Serialization.spaceSep (Lists.cons (Serialization.cst "hiding ") [
                Serialization.parens (Serialization.commaSep Serialization.inlineStyle (Lists.map importExportSpecToExpr v1))]))) spec)
      parts = (Maybes.cat [
              Just (Serialization.cst "import"),
              Logic.ifElse qual (Just (Serialization.cst "qualified")) Nothing,
              Just (Serialization.cst name),
              Maybes.map (\m -> Serialization.cst (Strings.cat2 "as " (Ast_.unModuleName m))) mod,
              (Maybes.map hidingSec mspec)])
  in (Serialization.spaceSep parts)

lambdaExpressionToExpr :: (Ast_.LambdaExpression -> Ast.Expr)
lambdaExpressionToExpr lambdaExpr =  
  let bindings = (Ast_.lambdaExpressionBindings lambdaExpr) 
      inner = (Ast_.lambdaExpressionInner lambdaExpr)
      head = (Serialization.spaceSep (Lists.map patternToExpr bindings))
      body = (expressionToExpr inner)
  in (Serialization.ifx Operators.lambdaOp (Serialization.prefix "\\" head) body)

literalToExpr :: (Ast_.Literal -> Ast.Expr)
literalToExpr lit =  
  let parensIfNeg = (\b -> \e -> Logic.ifElse b (Strings.cat [
          "(",
          e,
          ")"]) e)
  in (Serialization.cst ((\x -> case x of
    Ast_.LiteralChar v1 -> (Literals.showString (Literals.showUint16 v1))
    Ast_.LiteralDouble v1 -> (parensIfNeg (Equality.lt v1 0.0) (Literals.showFloat64 v1))
    Ast_.LiteralFloat v1 -> (parensIfNeg (Equality.lt v1 0.0) (Literals.showFloat32 v1))
    Ast_.LiteralInt v1 -> (parensIfNeg (Equality.lt v1 0) (Literals.showInt32 v1))
    Ast_.LiteralInteger v1 -> (parensIfNeg (Equality.lt v1 0) (Literals.showBigint v1))
    Ast_.LiteralString v1 -> (Literals.showString v1)) lit))

localBindingToExpr :: (Ast_.LocalBinding -> Ast.Expr)
localBindingToExpr binding = ((\x -> case x of
  Ast_.LocalBindingSignature v1 -> (typeSignatureToExpr v1)
  Ast_.LocalBindingValue v1 -> (valueBindingToExpr v1)) binding)

moduleHeadToExpr :: (Ast_.ModuleHead -> Ast.Expr)
moduleHeadToExpr moduleHead =  
  let mc = (Ast_.moduleHeadComments moduleHead) 
      modName = (Ast_.moduleHeadName moduleHead)
      mname = (Ast_.unModuleName modName)
      head = (Serialization.spaceSep (Lists.cons (Serialization.cst "module") (Lists.cons (Serialization.cst mname) [
              Serialization.cst "where"])))
  in (Maybes.maybe head (\c -> Serialization.newlineSep (Lists.cons (Serialization.cst (toHaskellComments c)) (Lists.cons (Serialization.cst "") [
    head]))) mc)

moduleToExpr :: (Ast_.Module -> Ast.Expr)
moduleToExpr module_ =  
  let mh = (Ast_.moduleHead module_) 
      imports = (Ast_.moduleImports module_)
      decls = (Ast_.moduleDeclarations module_)
      warning = [
              Serialization.cst (toSimpleComments Constants.warningAutoGeneratedFile)]
      headerLine = (Maybes.maybe [] (\h -> [
              moduleHeadToExpr h]) mh)
      declLines = (Lists.map declarationWithCommentsToExpr decls)
      importLines = (Logic.ifElse (Lists.null imports) [] [
              Serialization.newlineSep (Lists.map importToExpr imports)])
  in (Serialization.doubleNewlineSep (Lists.concat [
    warning,
    headerLine,
    importLines,
    declLines]))

nameToExpr :: (Ast_.Name -> Ast.Expr)
nameToExpr name = (Serialization.cst ((\x -> case x of
  Ast_.NameImplicit v1 -> (Strings.cat2 "?" (writeQualifiedName v1))
  Ast_.NameNormal v1 -> (writeQualifiedName v1)
  Ast_.NameParens v1 -> (Strings.cat [
    "(",
    writeQualifiedName v1,
    ")"])) name))

patternToExpr :: (Ast_.Pattern -> Ast.Expr)
patternToExpr pat = ((\x -> case x of
  Ast_.PatternApplication v1 -> (applicationPatternToExpr v1)
  Ast_.PatternList v1 -> (Serialization.bracketList Serialization.halfBlockStyle (Lists.map patternToExpr v1))
  Ast_.PatternLiteral v1 -> (literalToExpr v1)
  Ast_.PatternName v1 -> (nameToExpr v1)
  Ast_.PatternParens v1 -> (Serialization.parenthesize (patternToExpr v1))
  Ast_.PatternTuple v1 -> (Serialization.parenList False (Lists.map patternToExpr v1))
  Ast_.PatternWildcard -> (Serialization.cst "_")) pat)

rightHandSideToExpr :: (Ast_.RightHandSide -> Ast.Expr)
rightHandSideToExpr rhs = (expressionToExpr (Ast_.unRightHandSide rhs))

statementToExpr :: (Ast_.Statement -> Ast.Expr)
statementToExpr stmt = (expressionToExpr (Ast_.unStatement stmt))

typeSignatureToExpr :: (Ast_.TypeSignature -> Ast.Expr)
typeSignatureToExpr typeSig =  
  let name = (Ast_.typeSignatureName typeSig) 
      typ = (Ast_.typeSignatureType typeSig)
  in (Serialization.spaceSep (Lists.cons (nameToExpr name) (Lists.cons (Serialization.cst "::") [
    typeToExpr typ])))

typeToExpr :: (Ast_.Type -> Ast.Expr)
typeToExpr htype = ((\x -> case x of
  Ast_.TypeApplication v1 ->  
    let lhs = (Ast_.applicationTypeContext v1) 
        rhs = (Ast_.applicationTypeArgument v1)
    in (Serialization.ifx Operators.appOp (typeToExpr lhs) (typeToExpr rhs))
  Ast_.TypeCtx v1 ->  
    let ctx = (Ast_.contextTypeCtx v1) 
        typ = (Ast_.contextTypeType v1)
    in (Serialization.ifx Operators.assertOp (assertionToExpr ctx) (typeToExpr typ))
  Ast_.TypeFunction v1 ->  
    let dom = (Ast_.functionTypeDomain v1) 
        cod = (Ast_.functionTypeCodomain v1)
    in (Serialization.ifx Operators.arrowOp (typeToExpr dom) (typeToExpr cod))
  Ast_.TypeList v1 -> (Serialization.bracketList Serialization.inlineStyle [
    typeToExpr v1])
  Ast_.TypeTuple v1 -> (Serialization.parenList False (Lists.map typeToExpr v1))
  Ast_.TypeVariable v1 -> (nameToExpr v1)) htype)

valueBindingToExpr :: (Ast_.ValueBinding -> Ast.Expr)
valueBindingToExpr vb = ((\x -> case x of
  Ast_.ValueBindingSimple v1 ->  
    let pat = (Ast_.simpleValueBindingPattern v1) 
        rhs = (Ast_.simpleValueBindingRhs v1)
        local = (Ast_.simpleValueBindingLocalBindings v1)
        body = (Serialization.ifx Operators.defineOp (patternToExpr pat) (rightHandSideToExpr rhs))
    in (Maybes.maybe body (\localBindings ->  
      let bindings = (Ast_.unLocalBindings localBindings)
      in (Serialization.indentBlock (Lists.cons body [
        Serialization.indentBlock (Lists.cons (Serialization.cst "where") (Lists.map localBindingToExpr bindings))]))) local)) vb)

variableToExpr :: (Ast_.Variable -> Ast.Expr)
variableToExpr variable = (nameToExpr (Ast_.unVariable variable))

toHaskellComments :: (String -> String)
toHaskellComments c = (Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- | " s) (Strings.lines c)))

toSimpleComments :: (String -> String)
toSimpleComments c = (Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "-- " s) (Strings.lines c)))

writeQualifiedName :: (Ast_.QualifiedName -> String)
writeQualifiedName qname =  
  let qualifiers = (Ast_.qualifiedNameQualifiers qname) 
      unqual = (Ast_.qualifiedNameUnqualified qname)
      h = (\namePart -> Ast_.unNamePart namePart)
      allParts = (Lists.concat2 (Lists.map h qualifiers) [
              h unqual])
  in (Strings.intercalate "." allParts)
