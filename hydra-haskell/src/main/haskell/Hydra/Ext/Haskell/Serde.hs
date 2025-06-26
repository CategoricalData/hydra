-- | Haskell operator precendence and associativity are drawn from:
--   https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- Other operators were investigated using GHCi, e.g. ":info (->)"
-- Operator names are drawn (loosely) from:
--   https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Ext.Haskell.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Ext.Haskell.Operators as Operators
import qualified Hydra.Serialization as Serialization

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


alternativeToExpr :: H.Alternative -> Ast.Expr
alternativeToExpr (H.Alternative pat rhs _) = Serialization.ifx Operators.caseOp (patternToExpr pat) (caseRhsToExpr rhs)

applicationExpressionToExpr :: H.ApplicationExpression -> Ast.Expr
applicationExpressionToExpr (H.ApplicationExpression fun arg) = Serialization.ifx Operators.appOp (expressionToExpr fun) (expressionToExpr arg)

applicationPatternToExpr :: H.ApplicationPattern -> Ast.Expr
applicationPatternToExpr (H.ApplicationPattern name pats) =
  Serialization.spaceSep $ nameToExpr name : (patternToExpr <$> pats)

assertionToExpr :: H.Assertion -> Ast.Expr
assertionToExpr sert = case sert of
  H.AssertionClass cls -> classAssertionToExpr cls
  H.AssertionTuple serts -> Serialization.parenList False (assertionToExpr <$> serts)

caseExpressionToExpr :: H.CaseExpression -> Ast.Expr
caseExpressionToExpr (H.CaseExpression cs alts) = Serialization.ifx ofOp lhs rhs
  where
    lhs = Serialization.spaceSep [Serialization.cst "case", expressionToExpr cs]
    rhs = Serialization.newlineSep (alternativeToExpr <$> alts)
    ofOp = Ast.Op (Ast.Symbol "of") (Ast.Padding Ast.WsSpace $ Ast.WsBreakAndIndent "  ") (Ast.Precedence 0) Ast.AssociativityNone

caseRhsToExpr :: H.CaseRhs -> Ast.Expr
caseRhsToExpr (H.CaseRhs expr) = expressionToExpr expr

classAssertionToExpr :: H.ClassAssertion -> Ast.Expr
classAssertionToExpr (H.ClassAssertion name types) =
  Serialization.spaceSep [nameToExpr name, Serialization.commaSep Serialization.halfBlockStyle (typeToExpr <$> types)]

constructorToExpr :: H.Constructor -> Ast.Expr
constructorToExpr cons = case cons of
  H.ConstructorOrdinary (H.OrdinaryConstructor name types) ->
    Serialization.spaceSep [nameToExpr name, Serialization.spaceSep (typeToExpr <$> types)]
  H.ConstructorRecord (H.RecordConstructor name fields) ->
    Serialization.spaceSep [nameToExpr name, Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (fieldWithCommentsToExpr <$> fields)]

constructorWithCommentsToExpr :: H.ConstructorWithComments -> Ast.Expr
constructorWithCommentsToExpr (H.ConstructorWithComments body mc) = case mc of
  Nothing -> constructorToExpr body
  Just c -> Serialization.newlineSep [Serialization.cst $ toHaskellComments c, constructorToExpr body]

dataOrNewtypeToExpr :: H.DataOrNewtype -> Ast.Expr
dataOrNewtypeToExpr kw = case kw of
  H.DataOrNewtypeData -> Serialization.cst "data"
  H.DataOrNewtypeNewtype -> Serialization.cst "newtype"

declarationHeadToExpr :: H.DeclarationHead -> Ast.Expr
declarationHeadToExpr hd = case hd of
  H.DeclarationHeadApplication (H.ApplicationDeclarationHead fun op) -> Serialization.spaceSep [declarationHeadToExpr fun, variableToExpr op]
  -- H.DeclarationHeadParens not handled in original; ignoring
  H.DeclarationHeadSimple name -> nameToExpr name

declarationToExpr :: H.Declaration -> Ast.Expr
declarationToExpr decl = case decl of
  H.DeclarationData (H.DataDeclaration kw _ hd cons deriv) -> Serialization.indentBlock $
      [Serialization.spaceSep [dataOrNewtypeToExpr kw, declarationHeadToExpr hd, Serialization.cst "="], constructors]
      ++ if L.null derivCat then [] else [Serialization.spaceSep [Serialization.cst "deriving", Serialization.parenList False (nameToExpr <$> derivCat)]]
    where
      derivCat = L.concat $ h <$> deriv
        where
          h (H.Deriving names) = names
      constructors = Serialization.orSep Serialization.halfBlockStyle (constructorWithCommentsToExpr <$> cons)
  H.DeclarationType (H.TypeDeclaration hd typ) -> Serialization.spaceSep [Serialization.cst "type", declarationHeadToExpr hd, Serialization.cst "=", typeToExpr typ]
  H.DeclarationValueBinding vb -> valueBindingToExpr vb
  H.DeclarationTypedBinding (H.TypedBinding (H.TypeSignature name htype) vb) -> Serialization.newlineSep [
      Serialization.ifx Operators.typeOp (nameToExpr name) (typeToExpr htype),
      valueBindingToExpr vb]

declarationWithCommentsToExpr :: H.DeclarationWithComments -> Ast.Expr
declarationWithCommentsToExpr (H.DeclarationWithComments body mc) = case mc of
  Nothing -> declarationToExpr body
  Just c -> Serialization.newlineSep [Serialization.cst $ toHaskellComments c, declarationToExpr body]

expressionToExpr :: H.Expression -> Ast.Expr
expressionToExpr expr = case expr of
    H.ExpressionApplication app -> applicationExpressionToExpr app
    H.ExpressionCase cases -> caseExpressionToExpr cases
    H.ExpressionConstructRecord r -> constructRecordExpressionToExpr r
    H.ExpressionDo statements -> Serialization.indentBlock $ [Serialization.cst "do"] ++ (statementToExpr <$> statements)
    H.ExpressionIf ifte -> ifExpressionToExpr ifte
    -- H.ExpressionInfixApplication skipped as in original
    H.ExpressionLiteral lit -> literalToExpr lit
    -- Note: the need for extra parens may point to an operator precedence issue
    H.ExpressionLambda lam -> Serialization.parenthesize $ lambdaExpressionToExpr lam
    -- H.ExpressionLeftSection skipped
    H.ExpressionLet (H.LetExpression bindings inner) -> Serialization.indentBlock [
        Serialization.cst "",
        Serialization.spaceSep [Serialization.cst "let", Serialization.customIndentBlock "    " (encodeBinding <$> bindings)],
        Serialization.spaceSep [Serialization.cst "in", expressionToExpr inner]]
      where
        encodeBinding = Serialization.indentSubsequentLines "      " . localBindingToExpr
    H.ExpressionList exprs -> Serialization.bracketList Serialization.halfBlockStyle $ expressionToExpr <$> exprs
    H.ExpressionParens expr' -> Serialization.parenthesize $ expressionToExpr expr'
    -- H.ExpressionPrefixApplication skipped
    -- H.ExpressionRightSection skipped
    H.ExpressionTuple exprs -> Serialization.parenList False $ expressionToExpr <$> exprs
    -- H.ExpressionTypeSignature skipped
    -- H.ExpressionUpdateRecord skipped
    H.ExpressionVariable name -> nameToExpr name

constructRecordExpressionToExpr :: H.ConstructRecordExpression -> Ast.Expr
constructRecordExpressionToExpr (H.ConstructRecordExpression name updates) =
  Serialization.spaceSep [nameToExpr name, Serialization.brackets Serialization.curlyBraces Serialization.halfBlockStyle body]
  where
    body = Serialization.commaSep Serialization.halfBlockStyle (fromUpdate <$> updates)
    fromUpdate (H.FieldUpdate fn val) = Serialization.ifx Operators.defineOp (nameToExpr fn) (expressionToExpr val)

fieldToExpr :: H.Field -> Ast.Expr
fieldToExpr (H.Field name typ) = Serialization.spaceSep [nameToExpr name, Serialization.cst "::", typeToExpr typ]

fieldWithCommentsToExpr :: H.FieldWithComments -> Ast.Expr
fieldWithCommentsToExpr (H.FieldWithComments field mc) = case mc of
  Nothing -> fieldToExpr field
  Just c -> Serialization.newlineSep [Serialization.cst $ toHaskellComments c, fieldToExpr field]

ifExpressionToExpr :: H.IfExpression -> Ast.Expr
ifExpressionToExpr (H.IfExpression eif ethen eelse) = Serialization.ifx ifOp (Serialization.spaceSep [Serialization.cst "if", expressionToExpr eif]) body
  where
    ifOp = Ast.Op (Ast.Symbol "") (Ast.Padding Ast.WsNone $ Ast.WsBreakAndIndent "  ") (Ast.Precedence 0) Ast.AssociativityNone
    body = Serialization.newlineSep [Serialization.spaceSep [Serialization.cst "then", expressionToExpr ethen], Serialization.spaceSep [Serialization.cst "else", expressionToExpr eelse]]

importExportSpecToExpr :: H.ImportExportSpec -> Ast.Expr
importExportSpecToExpr (H.ImportExportSpec _ name _) = nameToExpr name

importToExpr :: H.Import -> Ast.Expr
importToExpr (H.Import qual (H.ModuleName name) mod mspec) = Serialization.spaceSep $ Y.catMaybes [
    Just $ Serialization.cst "import",
    if qual then Just (Serialization.cst "qualified") else Nothing,
    Just $ Serialization.cst name,
    (\(H.ModuleName m) -> Serialization.cst $ "as " ++ m) <$> mod,
    fmap hidingSec mspec]
  where
    hidingSec (H.SpecImportHiding names) = Serialization.spaceSep [
      Serialization.cst $ "hiding ",
      Serialization.parens $ Serialization.commaSep Serialization.inlineStyle (importExportSpecToExpr <$> names)]

lambdaExpressionToExpr :: H.LambdaExpression -> Ast.Expr
lambdaExpressionToExpr (H.LambdaExpression bindings inner) = Serialization.ifx Operators.lambdaOp (Serialization.prefix "\\" head) body
  where
    head = Serialization.spaceSep (patternToExpr <$> bindings)
    body = expressionToExpr inner

literalToExpr :: H.Literal -> Ast.Expr
literalToExpr lit = Serialization.cst $ case lit of
  H.LiteralChar c -> show $ C.chr $ fromIntegral c
  H.LiteralDouble d -> if d < 0 then "(0" ++ show d ++ ")" else show d
  H.LiteralFloat f -> if f < 0 then "(0" ++ show f ++ ")" else show f
  H.LiteralInt i -> if i < 0 then "(0" ++ show i ++ ")" else show i
  H.LiteralInteger i -> show i
  H.LiteralString s -> show s

localBindingToExpr :: H.LocalBinding -> Ast.Expr
localBindingToExpr binding = case binding of
  H.LocalBindingSignature ts -> typeSignatureToExpr ts
  H.LocalBindingValue vb -> valueBindingToExpr vb

moduleHeadToExpr :: H.ModuleHead -> Ast.Expr
moduleHeadToExpr (H.ModuleHead mc (H.ModuleName mname) _) = case mc of
  Nothing -> head
  Just c -> Serialization.newlineSep [Serialization.cst $ toHaskellComments c, Serialization.cst "", head]
  where
    head = Serialization.spaceSep [Serialization.cst "module", Serialization.cst mname, Serialization.cst "where"]

moduleToExpr :: H.Module -> Ast.Expr
moduleToExpr (H.Module mh imports decls) = Serialization.doubleNewlineSep $
    headerLine ++ importLines ++ declLines
  where
    headerLine = maybe [] (\h -> [moduleHeadToExpr h]) mh
    declLines = declarationWithCommentsToExpr <$> decls
    importLines = [Serialization.newlineSep $ importToExpr <$> imports | not (L.null imports)]

nameToExpr :: H.Name -> Ast.Expr
nameToExpr name = Serialization.cst $ case name of
  H.NameImplicit qn -> "?" ++ writeQualifiedName qn
  H.NameNormal qn -> writeQualifiedName qn
  H.NameParens qn -> "(" ++ writeQualifiedName qn ++ ")"

patternToExpr :: H.Pattern -> Ast.Expr
patternToExpr pat = case pat of
    H.PatternApplication app -> applicationPatternToExpr app
    -- H.PatternAs skipped
    H.PatternList pats -> Serialization.bracketList Serialization.halfBlockStyle $ patternToExpr <$> pats
    H.PatternLiteral lit -> literalToExpr lit
    H.PatternName name -> nameToExpr name
    H.PatternParens pat' -> Serialization.parenthesize $ patternToExpr pat'
    -- H.PatternRecord skipped
    H.PatternTuple pats -> Serialization.parenList False $ patternToExpr <$> pats
    -- H.PatternTyped skipped
    H.PatternWildcard -> Serialization.cst "_"

rightHandSideToExpr :: H.RightHandSide -> Ast.Expr
rightHandSideToExpr (H.RightHandSide expr) = expressionToExpr expr

statementToExpr :: H.Statement -> Ast.Expr
statementToExpr (H.Statement expr) = expressionToExpr expr

typeSignatureToExpr :: H.TypeSignature -> Ast.Expr
typeSignatureToExpr (H.TypeSignature name typ) = Serialization.spaceSep [nameToExpr name, Serialization.cst "::", typeToExpr typ]

typeToExpr :: H.Type -> Ast.Expr
typeToExpr htype = case htype of
  H.TypeApplication (H.ApplicationType lhs rhs) -> Serialization.ifx Operators.appOp (typeToExpr lhs) (typeToExpr rhs)
  H.TypeCtx (H.ContextType ctx typ) -> Serialization.ifx Operators.assertOp (assertionToExpr ctx) (typeToExpr typ)
  H.TypeFunction (H.FunctionType dom cod) -> Serialization.ifx Operators.arrowOp (typeToExpr dom) (typeToExpr cod)
  -- H.TypeInfix skipped
  H.TypeList htype' -> Serialization.bracketList Serialization.inlineStyle [typeToExpr htype']
  -- H.TypeParens skipped
  H.TypeTuple types -> Serialization.parenList False $ typeToExpr <$> types
  H.TypeVariable name -> nameToExpr name

valueBindingToExpr :: H.ValueBinding -> Ast.Expr
valueBindingToExpr vb = case vb of
  H.ValueBindingSimple (H.SimpleValueBinding pat rhs local) -> case local of
      Nothing -> body
      Just (H.LocalBindings bindings) -> Serialization.indentBlock [body, Serialization.indentBlock $ (Serialization.cst "where") : (localBindingToExpr <$> bindings)]
    where
      body = Serialization.ifx Operators.defineOp (patternToExpr pat) (rightHandSideToExpr rhs)

variableToExpr :: H.Variable -> Ast.Expr
variableToExpr (H.Variable v) = nameToExpr v


-- Helpers from original module:

toHaskellComments :: String -> String
toHaskellComments c = L.intercalate "\n" $ ("-- | " ++) <$> L.lines c

writeQualifiedName :: H.QualifiedName -> String
writeQualifiedName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ (h <$> qualifiers) ++ [h unqual]
  where
    h (H.NamePart part) = part