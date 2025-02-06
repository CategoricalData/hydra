-- | Haskell operator precendence and associativity are drawn from:
--   https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- Other operators were investigated using GHCi, e.g. ":info (->)"
-- Operator names are drawn (loosely) from:
--   https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Ext.Haskell.Serde where

import Hydra.Ast
import Hydra.Staging.Serialization
import Hydra.Ext.Haskell.Operators
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


class ToTree a where
  toTree :: a -> Expr

instance ToTree H.Alternative where
  toTree (H.Alternative pat rhs _) = ifx caseOp (toTree pat) (toTree rhs)

instance ToTree H.Assertion where
  toTree sert = case sert of
    H.AssertionClass cls -> toTree cls
    H.AssertionTuple serts -> parenList False (toTree <$> serts)

instance ToTree H.Assertion_Class where
  toTree (H.Assertion_Class name types) = spaceSep [toTree name, commaSep halfBlockStyle (toTree <$> types)]

instance ToTree H.CaseRhs where
  toTree (H.CaseRhs expr) = toTree expr

instance ToTree H.Constructor where
  toTree cons = case cons of
    H.ConstructorOrdinary (H.Constructor_Ordinary name types) -> spaceSep [
      toTree name,
      spaceSep (toTree <$> types)]
    H.ConstructorRecord (H.Constructor_Record name fields) -> spaceSep [
      toTree name,
      curlyBracesList Nothing halfBlockStyle (toTree <$> fields)]

instance ToTree H.ConstructorWithComments where
  toTree (H.ConstructorWithComments body mc) = case mc of
    Nothing -> toTree body
    Just c -> newlineSep [cst $ toHaskellComments c, toTree body]

instance ToTree H.DataDeclaration_Keyword where
  toTree kw = case kw of
    H.DataDeclaration_KeywordData -> cst "data"
    H.DataDeclaration_KeywordNewtype -> cst "newtype"

instance ToTree H.Declaration where
  toTree decl = case decl of
    H.DeclarationData (H.DataDeclaration kw _ hd cons deriv) -> indentBlock $
        [spaceSep [toTree kw, toTree hd, cst "="], constructors]
        ++ if L.null derivCat then [] else [spaceSep [cst "deriving", parenList False (toTree <$> derivCat)]]
      where
        derivCat = L.concat $ h <$> deriv
          where
            h (H.Deriving names) = names
        constructors = orSep halfBlockStyle (toTree <$> cons)
    H.DeclarationType (H.TypeDeclaration hd typ) -> spaceSep [cst "type", toTree hd, cst "=", toTree typ]
    H.DeclarationValueBinding vb -> toTree vb
    H.DeclarationTypedBinding (H.TypedBinding (H.TypeSignature name htype) vb) -> newlineSep [ -- TODO: local bindings
        ifx typeOp (toTree name) (toTree htype),
        toTree vb]

instance ToTree H.DeclarationHead where
  toTree hd = case hd of
    H.DeclarationHeadApplication (H.DeclarationHead_Application fun op) -> spaceSep [toTree fun, toTree op]
--    H.DeclarationHeadParens ... ->
    H.DeclarationHeadSimple name -> toTree name

instance ToTree H.DeclarationWithComments where
  toTree (H.DeclarationWithComments body mc) = case mc of
    Nothing -> toTree body
    Just c -> newlineSep [cst $ toHaskellComments c, toTree body]

instance ToTree H.Expression where
  toTree expr = case expr of
      H.ExpressionApplication app -> toTree app
      H.ExpressionCase cases -> toTree cases
      H.ExpressionConstructRecord r -> toTree r
      H.ExpressionDo statements -> indentBlock $ [cst "do"] ++ (toTree <$> statements)
      H.ExpressionIf ifte -> toTree ifte
    --  H.ExpressionInfixApplication Term_InfixApplication
      H.ExpressionLiteral lit -> toTree lit
      -- Note: the need for extra parens may point to an operator precedence issue
      H.ExpressionLambda lam -> parenthesize $ toTree lam
    --  H.ExpressionLeftSection Term_Section
      H.ExpressionLet (H.Expression_Let bindings inner) -> indentBlock [
          cst "",
          spaceSep [cst "let", customIndentBlock "    " (encodeBinding <$> bindings)],
          spaceSep [cst "in", toTree inner]]
        where
          -- Note: indentation should depend on the length of the pattern
          encodeBinding = indentSubsequentLines "      " . toTree
      H.ExpressionList exprs -> bracketList halfBlockStyle $ toTree <$> exprs
      H.ExpressionParens expr' -> parenthesize $ toTree expr'
    --  H.ExpressionPrefixApplication Term_PrefixApplication
    --  H.ExpressionRightSection Term_Section
      H.ExpressionTuple exprs -> parenList False $ toTree <$> exprs
    --  H.ExpressionTypeSignature Term_TypeSignature
    --  H.ExpressionUpdateRecord Term_UpdateRecord
      H.ExpressionVariable name -> toTree name

instance ToTree H.Expression_Application where
  toTree (H.Expression_Application fun arg) = ifx appOp (toTree fun) (toTree arg)

instance ToTree H.Expression_Case where
  toTree (H.Expression_Case cs alts) = ifx ofOp lhs rhs
    where
      lhs = spaceSep [cst "case", toTree cs]
      rhs = newlineSep (toTree <$> alts)
      ofOp = Op (Symbol "of") (Padding WsSpace $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone

instance ToTree H.Expression_ConstructRecord where
  toTree (H.Expression_ConstructRecord name updates) = spaceSep [toTree name, brackets curlyBraces halfBlockStyle body]
    where
      body = commaSep halfBlockStyle (fromUpdate <$> updates)
      fromUpdate (H.FieldUpdate fn val) = ifx defineOp (toTree fn) (toTree val)

instance ToTree H.Expression_If where
  toTree (H.Expression_If eif ethen eelse) = ifx ifOp (spaceSep [cst "if", toTree eif]) body
    where
      ifOp = Op (Symbol "") (Padding WsNone $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone
      body = newlineSep [spaceSep [cst "then", toTree ethen], spaceSep [cst "else", toTree eelse]]

instance ToTree H.Expression_Lambda where
  toTree (H.Expression_Lambda bindings inner) = ifx lambdaOp (prefix "\\" head) body
    where
      head = spaceSep (toTree <$> bindings)
      body = toTree inner

instance ToTree H.Field where
  toTree (H.Field name typ) = spaceSep [toTree name, cst "::", toTree typ]

instance ToTree H.FieldWithComments where
  toTree (H.FieldWithComments field mc) = case mc of
      Nothing -> toTree field
      Just c -> newlineSep [cst $ toHaskellComments c, toTree field]

instance ToTree H.Import where
  toTree (H.Import qual (H.ModuleName name) mod _) = spaceSep $ Y.catMaybes [
      Just $ cst "import",
      if qual then Just (cst "qualified") else Nothing,
      Just $ cst name,
      (\(H.ModuleName m) -> cst $ "as " ++ m) <$> mod]

instance ToTree H.Literal where
  toTree lit = cst $ case lit of
    H.LiteralChar c -> show $ C.chr $ fromIntegral c
    H.LiteralDouble d -> if d < 0 then "(0" ++ show d ++ ")" else show d
    H.LiteralFloat f -> if f < 0 then "(0" ++ show f ++ ")" else show f
    H.LiteralInt i -> if i < 0 then "(0" ++ show i ++ ")" else show i
    H.LiteralInteger i -> show i
    H.LiteralString s -> show s

instance ToTree H.LocalBinding where
  toTree binding = case binding of
    H.LocalBindingSignature ts -> toTree ts
    H.LocalBindingValue vb -> toTree vb

instance ToTree H.Module where
  toTree (H.Module mh imports decls) = doubleNewlineSep $
      headerLine ++ importLines ++ declLines
    where
      headerLine = Y.maybe [] (\h -> [toTree h]) mh
      declLines = toTree <$> decls
      importLines = [newlineSep $ toTree <$> imports | not (L.null imports)]

instance ToTree H.Name where
  toTree name = cst $ case name of
    H.NameImplicit qn -> "?" ++ writeQualifiedName qn
    H.NameNormal qn -> writeQualifiedName qn
    H.NameParens qn -> "(" ++ writeQualifiedName qn ++ ")"

instance ToTree H.ModuleHead where
  toTree (H.ModuleHead mc (H.ModuleName mname) _) = case mc of
    Nothing -> head
    Just c -> newlineSep [cst $ toHaskellComments c, cst "", head]
    where
      head = spaceSep [cst "module", cst mname, cst "where"]

instance ToTree H.Pattern where
  toTree pat = case pat of
      H.PatternApplication app -> toTree app
--      H.PatternAs (H.Pattern_As ) ->
      H.PatternList pats -> bracketList halfBlockStyle $ toTree <$> pats
      H.PatternLiteral lit -> toTree lit
      H.PatternName name -> toTree name
      H.PatternParens pat -> parenthesize $ toTree pat
--      H.PatternRecord (H.Pattern_Record ) ->
      H.PatternTuple pats -> parenList False $ toTree <$> pats
--      H.PatternTyped (H.Pattern_Typed ) ->
      H.PatternWildcard -> cst "_"

instance ToTree H.Pattern_Application where
  toTree (H.Pattern_Application name pats) = spaceSep $ toTree name:(toTree <$> pats)

instance ToTree H.RightHandSide where
  toTree (H.RightHandSide expr) = toTree expr

instance ToTree H.Statement where
  toTree (H.Statement expr) = toTree expr

instance ToTree H.Type where
  toTree htype = case htype of
    H.TypeApplication (H.Type_Application lhs rhs) -> ifx appOp (toTree lhs) (toTree rhs)
    H.TypeCtx (H.Type_Context ctx typ) -> ifx assertOp (toTree ctx) (toTree typ)
    H.TypeFunction (H.Type_Function dom cod) -> ifx arrowOp (toTree dom) (toTree cod)
--  H.TypeInfix Type_Infix
    H.TypeList htype -> bracketList inlineStyle [toTree htype]
--  H.TypeParens Type
    H.TypeTuple types -> parenList False $ toTree <$> types
    H.TypeVariable name -> toTree name

instance ToTree H.TypeSignature where
  toTree (H.TypeSignature name typ) = spaceSep [toTree name, cst "::", toTree typ]

instance ToTree H.ValueBinding where
  toTree vb = case vb of
    H.ValueBindingSimple (H.ValueBinding_Simple pat rhs local) -> case local of
        Nothing -> body
        Just (H.LocalBindings bindings) -> indentBlock [body, indentBlock $ [cst "where"] ++ (toTree <$> bindings)]
      where
        body = ifx defineOp (toTree pat) (toTree rhs)

instance ToTree H.Variable where
  toTree (H.Variable v) = toTree v

toHaskellComments :: String -> String
toHaskellComments c = L.intercalate "\n" $ ("-- | " ++) <$> L.lines c

writeQualifiedName :: H.QualifiedName -> String
writeQualifiedName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ (h <$> qualifiers) ++ [h unqual]
  where
    h (H.NamePart part) = part
