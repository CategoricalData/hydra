-- | Haskell operator precendence and associativity are drawn from:
--   https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- Other operators were investigated using GHCi, e.g. ":info (->)"
-- Operator names are drawn (loosely) from:
--   https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Ext.Haskell.Serde where

import Hydra.Ast
import Hydra.Serialization
import Hydra.Ext.Haskell.Operators
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


alternativeToExpr :: H.Alternative -> Expr
alternativeToExpr (H.Alternative pat rhs _) = ifx caseOp (patternToExpr pat) (caseRhsToExpr rhs)

applicationExpressionToExpr :: H.ApplicationExpression -> Expr
applicationExpressionToExpr (H.ApplicationExpression fun arg) = ifx appOp (expressionToExpr fun) (expressionToExpr arg)

applicationPatternToExpr :: H.ApplicationPattern -> Expr
applicationPatternToExpr (H.ApplicationPattern name pats) =
  spaceSep $ nameToExpr name : (patternToExpr <$> pats)

assertionToExpr :: H.Assertion -> Expr
assertionToExpr sert = case sert of
  H.AssertionClass cls -> classAssertionToExpr cls
  H.AssertionTuple serts -> parenList False (assertionToExpr <$> serts)

caseExpressionToExpr :: H.CaseExpression -> Expr
caseExpressionToExpr (H.CaseExpression cs alts) = ifx ofOp lhs rhs
  where
    lhs = spaceSep [cst "case", expressionToExpr cs]
    rhs = newlineSep (alternativeToExpr <$> alts)
    ofOp = Op (Symbol "of") (Padding WsSpace $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone

caseRhsToExpr :: H.CaseRhs -> Expr
caseRhsToExpr (H.CaseRhs expr) = expressionToExpr expr

classAssertionToExpr :: H.ClassAssertion -> Expr
classAssertionToExpr (H.ClassAssertion name types) =
  spaceSep [nameToExpr name, commaSep halfBlockStyle (typeToExpr <$> types)]

constructorToExpr :: H.Constructor -> Expr
constructorToExpr cons = case cons of
  H.ConstructorOrdinary (H.OrdinaryConstructor name types) ->
    spaceSep [nameToExpr name, spaceSep (typeToExpr <$> types)]
  H.ConstructorRecord (H.RecordConstructor name fields) ->
    spaceSep [nameToExpr name, curlyBracesList Nothing halfBlockStyle (fieldWithCommentsToExpr <$> fields)]

constructorWithCommentsToExpr :: H.ConstructorWithComments -> Expr
constructorWithCommentsToExpr (H.ConstructorWithComments body mc) = case mc of
  Nothing -> constructorToExpr body
  Just c -> newlineSep [cst $ toHaskellComments c, constructorToExpr body]

dataOrNewtypeToExpr :: H.DataOrNewtype -> Expr
dataOrNewtypeToExpr kw = case kw of
  H.DataOrNewtypeData -> cst "data"
  H.DataOrNewtypeNewtype -> cst "newtype"

declarationHeadToExpr :: H.DeclarationHead -> Expr
declarationHeadToExpr hd = case hd of
  H.DeclarationHeadApplication (H.ApplicationDeclarationHead fun op) -> spaceSep [declarationHeadToExpr fun, variableToExpr op]
  -- H.DeclarationHeadParens not handled in original; ignoring
  H.DeclarationHeadSimple name -> nameToExpr name

declarationToExpr :: H.Declaration -> Expr
declarationToExpr decl = case decl of
  H.DeclarationData (H.DataDeclaration kw _ hd cons deriv) -> indentBlock $
      [spaceSep [dataOrNewtypeToExpr kw, declarationHeadToExpr hd, cst "="], constructors]
      ++ if L.null derivCat then [] else [spaceSep [cst "deriving", parenList False (nameToExpr <$> derivCat)]]
    where
      derivCat = L.concat $ h <$> deriv
        where
          h (H.Deriving names) = names
      constructors = orSep halfBlockStyle (constructorWithCommentsToExpr <$> cons)
  H.DeclarationType (H.TypeDeclaration hd typ) -> spaceSep [cst "type", declarationHeadToExpr hd, cst "=", typeToExpr typ]
  H.DeclarationValueBinding vb -> valueBindingToExpr vb
  H.DeclarationTypedBinding (H.TypedBinding (H.TypeSignature name htype) vb) -> newlineSep [
      ifx typeOp (nameToExpr name) (typeToExpr htype),
      valueBindingToExpr vb]

declarationWithCommentsToExpr :: H.DeclarationWithComments -> Expr
declarationWithCommentsToExpr (H.DeclarationWithComments body mc) = case mc of
  Nothing -> declarationToExpr body
  Just c -> newlineSep [cst $ toHaskellComments c, declarationToExpr body]

expressionToExpr :: H.Expression -> Expr
expressionToExpr expr = case expr of
    H.ExpressionApplication app -> applicationExpressionToExpr app
    H.ExpressionCase cases -> caseExpressionToExpr cases
    H.ExpressionConstructRecord r -> constructRecordExpressionToExpr r
    H.ExpressionDo statements -> indentBlock $ [cst "do"] ++ (statementToExpr <$> statements)
    H.ExpressionIf ifte -> ifExpressionToExpr ifte
    -- H.ExpressionInfixApplication skipped as in original
    H.ExpressionLiteral lit -> literalToExpr lit
    -- Note: the need for extra parens may point to an operator precedence issue
    H.ExpressionLambda lam -> parenthesize $ lambdaExpressionToExpr lam
    -- H.ExpressionLeftSection skipped
    H.ExpressionLet (H.LetExpression bindings inner) -> indentBlock [
        cst "",
        spaceSep [cst "let", customIndentBlock "    " (encodeBinding <$> bindings)],
        spaceSep [cst "in", expressionToExpr inner]]
      where
        encodeBinding = indentSubsequentLines "      " . localBindingToExpr
    H.ExpressionList exprs -> bracketList halfBlockStyle $ expressionToExpr <$> exprs
    H.ExpressionParens expr' -> parenthesize $ expressionToExpr expr'
    -- H.ExpressionPrefixApplication skipped
    -- H.ExpressionRightSection skipped
    H.ExpressionTuple exprs -> parenList False $ expressionToExpr <$> exprs
    -- H.ExpressionTypeSignature skipped
    -- H.ExpressionUpdateRecord skipped
    H.ExpressionVariable name -> nameToExpr name

constructRecordExpressionToExpr :: H.ConstructRecordExpression -> Expr
constructRecordExpressionToExpr (H.ConstructRecordExpression name updates) =
  spaceSep [nameToExpr name, brackets curlyBraces halfBlockStyle body]
  where
    body = commaSep halfBlockStyle (fromUpdate <$> updates)
    fromUpdate (H.FieldUpdate fn val) = ifx defineOp (nameToExpr fn) (expressionToExpr val)

fieldToExpr :: H.Field -> Expr
fieldToExpr (H.Field name typ) = spaceSep [nameToExpr name, cst "::", typeToExpr typ]

fieldWithCommentsToExpr :: H.FieldWithComments -> Expr
fieldWithCommentsToExpr (H.FieldWithComments field mc) = case mc of
  Nothing -> fieldToExpr field
  Just c -> newlineSep [cst $ toHaskellComments c, fieldToExpr field]

ifExpressionToExpr :: H.IfExpression -> Expr
ifExpressionToExpr (H.IfExpression eif ethen eelse) = ifx ifOp (spaceSep [cst "if", expressionToExpr eif]) body
  where
    ifOp = Op (Symbol "") (Padding WsNone $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone
    body = newlineSep [spaceSep [cst "then", expressionToExpr ethen], spaceSep [cst "else", expressionToExpr eelse]]

importExportSpecToExpr :: H.ImportExportSpec -> Expr
importExportSpecToExpr (H.ImportExportSpec _ name _) = nameToExpr name

importToExpr :: H.Import -> Expr
importToExpr (H.Import qual (H.ModuleName name) mod mspec) = spaceSep $ Y.catMaybes [
    Just $ cst "import",
    if qual then Just (cst "qualified") else Nothing,
    Just $ cst name,
    (\(H.ModuleName m) -> cst $ "as " ++ m) <$> mod,
    fmap hidingSec mspec]
  where
    hidingSec (H.SpecImportHiding names) = spaceSep [
      cst $ "hiding ",
      parens $ commaSep inlineStyle (importExportSpecToExpr <$> names)]

lambdaExpressionToExpr :: H.LambdaExpression -> Expr
lambdaExpressionToExpr (H.LambdaExpression bindings inner) = ifx lambdaOp (prefix "\\" head) body
  where
    head = spaceSep (patternToExpr <$> bindings)
    body = expressionToExpr inner

literalToExpr :: H.Literal -> Expr
literalToExpr lit = cst $ case lit of
  H.LiteralChar c -> show $ C.chr $ fromIntegral c
  H.LiteralDouble d -> if d < 0 then "(0" ++ show d ++ ")" else show d
  H.LiteralFloat f -> if f < 0 then "(0" ++ show f ++ ")" else show f
  H.LiteralInt i -> if i < 0 then "(0" ++ show i ++ ")" else show i
  H.LiteralInteger i -> show i
  H.LiteralString s -> show s

localBindingToExpr :: H.LocalBinding -> Expr
localBindingToExpr binding = case binding of
  H.LocalBindingSignature ts -> typeSignatureToExpr ts
  H.LocalBindingValue vb -> valueBindingToExpr vb

moduleHeadToExpr :: H.ModuleHead -> Expr
moduleHeadToExpr (H.ModuleHead mc (H.ModuleName mname) _) = case mc of
  Nothing -> head
  Just c -> newlineSep [cst $ toHaskellComments c, cst "", head]
  where
    head = spaceSep [cst "module", cst mname, cst "where"]

moduleToExpr :: H.Module -> Expr
moduleToExpr (H.Module mh imports decls) = doubleNewlineSep $
    headerLine ++ importLines ++ declLines
  where
    headerLine = maybe [] (\h -> [moduleHeadToExpr h]) mh
    declLines = declarationWithCommentsToExpr <$> decls
    importLines = [newlineSep $ importToExpr <$> imports | not (L.null imports)]

nameToExpr :: H.Name -> Expr
nameToExpr name = cst $ case name of
  H.NameImplicit qn -> "?" ++ writeQualifiedName qn
  H.NameNormal qn -> writeQualifiedName qn
  H.NameParens qn -> "(" ++ writeQualifiedName qn ++ ")"

patternToExpr :: H.Pattern -> Expr
patternToExpr pat = case pat of
    H.PatternApplication app -> applicationPatternToExpr app
    -- H.PatternAs skipped
    H.PatternList pats -> bracketList halfBlockStyle $ patternToExpr <$> pats
    H.PatternLiteral lit -> literalToExpr lit
    H.PatternName name -> nameToExpr name
    H.PatternParens pat' -> parenthesize $ patternToExpr pat'
    -- H.PatternRecord skipped
    H.PatternTuple pats -> parenList False $ patternToExpr <$> pats
    -- H.PatternTyped skipped
    H.PatternWildcard -> cst "_"

rightHandSideToExpr :: H.RightHandSide -> Expr
rightHandSideToExpr (H.RightHandSide expr) = expressionToExpr expr

statementToExpr :: H.Statement -> Expr
statementToExpr (H.Statement expr) = expressionToExpr expr

typeSignatureToExpr :: H.TypeSignature -> Expr
typeSignatureToExpr (H.TypeSignature name typ) = spaceSep [nameToExpr name, cst "::", typeToExpr typ]

typeToExpr :: H.Type -> Expr
typeToExpr htype = case htype of
  H.TypeApplication (H.ApplicationType lhs rhs) -> ifx appOp (typeToExpr lhs) (typeToExpr rhs)
  H.TypeCtx (H.ContextType ctx typ) -> ifx assertOp (assertionToExpr ctx) (typeToExpr typ)
  H.TypeFunction (H.FunctionType dom cod) -> ifx arrowOp (typeToExpr dom) (typeToExpr cod)
  -- H.TypeInfix skipped
  H.TypeList htype' -> bracketList inlineStyle [typeToExpr htype']
  -- H.TypeParens skipped
  H.TypeTuple types -> parenList False $ typeToExpr <$> types
  H.TypeVariable name -> nameToExpr name

valueBindingToExpr :: H.ValueBinding -> Expr
valueBindingToExpr vb = case vb of
  H.ValueBindingSimple (H.SimpleValueBinding pat rhs local) -> case local of
      Nothing -> body
      Just (H.LocalBindings bindings) -> indentBlock [body, indentBlock $ (cst "where") : (localBindingToExpr <$> bindings)]
    where
      body = ifx defineOp (patternToExpr pat) (rightHandSideToExpr rhs)

variableToExpr :: H.Variable -> Expr
variableToExpr (H.Variable v) = nameToExpr v


-- Helpers from original module:

toHaskellComments :: String -> String
toHaskellComments c = L.intercalate "\n" $ ("-- | " ++) <$> L.lines c

writeQualifiedName :: H.QualifiedName -> String
writeQualifiedName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ (h <$> qualifiers) ++ [h unqual]
  where
    h (H.NamePart part) = part
