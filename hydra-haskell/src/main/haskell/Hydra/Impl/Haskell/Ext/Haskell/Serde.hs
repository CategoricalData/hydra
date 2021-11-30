module Hydra.Impl.Haskell.Ext.Haskell.Serde (
  haskellSerdeDebug,
  haskellSerdeStr,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Ext.Haskell.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Codetree.Print
import Hydra.Util.Codetree.Script
import Hydra.Impl.Haskell.Dsl
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Data.Char as C
import qualified Data.List as L


class ToTree a where
  toTree :: a -> CT.Expr

instance ToTree H.Alternative where
  toTree (H.Alternative pat rhs _) = ifx caseOp (toTree pat) (toTree rhs)

instance ToTree H.Expression where
  toTree expr = case expr of
      H.ExpressionApplication app -> toTree app
      H.ExpressionCase cases -> toTree cases
      H.ExpressionConstructRecord r -> toTree r
      H.ExpressionDo statements -> indentBlock (cst "do") $ toTree <$> statements
      H.ExpressionIf ifte -> toTree ifte
    --  H.ExpressionInfixApplication Expression_InfixApplication
      H.ExpressionLiteral lit -> toTree lit
      H.ExpressionLambda lam -> toTree lam
    --  H.ExpressionLeftSection Expression_Section
    --  H.ExpressionLet Expression_Let
      H.ExpressionList exprs -> hlist True $ toTree <$> exprs
      H.ExpressionParens expr' -> parenthesize $ toTree expr'
    --  H.ExpressionPrefixApplication Expression_PrefixApplication
    --  H.ExpressionRightSection Expression_Section
      H.ExpressionTuple exprs -> htuple $ toTree <$> exprs
    --  H.ExpressionTypeSignature Expression_TypeSignature
    --  H.ExpressionUpdateRecord Expression_UpdateRecord
      H.ExpressionVariable name -> toTree name

instance ToTree H.Expression_Application where
  toTree (H.Expression_Application fun arg) = ifx appOp (toTree fun) (toTree arg)

instance ToTree H.Expression_Case where
  toTree (H.Expression_Case cs alts) = ifx ofOp lhs rhs
    where
      lhs = spaceSep [cst "case", toTree cs]
      rhs = newlineSep (toTree <$> alts)
      ofOp = CT.Op "of" (CT.Padding CT.WsSpace CT.WsBreakAndIndent) 0 CT.AssociativityNone

instance ToTree H.Expression_ConstructRecord where
  toTree (H.Expression_ConstructRecord name updates) = spaceSep [toTree name, brackets curlyBraces body]
    where
      body = commaSep True (fromUpdate <$> updates)
      fromUpdate (H.FieldUpdate fn val) = ifx defineOp (toTree fn) (toTree val)

instance ToTree H.Expression_If where
  toTree (H.Expression_If eif ethen eelse) = ifx ifOp (spaceSep [cst "if", toTree eif]) body
    where
      ifOp = CT.Op "" (CT.Padding CT.WsNone CT.WsBreakAndIndent) 0 CT.AssociativityNone
      body = newlineSep [spaceSep [cst "then", toTree ethen], spaceSep [cst "else", toTree eelse]]

instance ToTree H.Expression_Lambda where
  toTree (H.Expression_Lambda bindings inner) = prefix "\\" $ ifx lambdaOp head body
    where
      head = spaceSep (toTree <$> bindings)
      body = toTree inner

instance ToTree H.Literal where
  toTree lit = cst $ case lit of
    H.LiteralChar c -> show $ C.chr $ fromIntegral c
    H.LiteralDouble d -> show d
    H.LiteralFloat f -> show f
    H.LiteralInt i -> show i
    H.LiteralInteger i -> show i
    H.LiteralString s -> show s

instance ToTree H.Name where
  toTree name = cst $ case name of
    H.NameImplicit qn -> "?" ++ writeQName qn
    H.NameNormal qn -> writeQName qn
    H.NameParens qn -> "(" ++ writeQName qn ++ ")"

instance ToTree H.Pattern where
  toTree pat = case pat of
      H.PatternApplication app -> toTree app
--      H.PatternAs (H.Pattern_As ) ->
      H.PatternList pats -> hlist True $ toTree <$> pats
      H.PatternLiteral lit -> toTree lit
      H.PatternName name -> toTree name
      H.PatternParens pat -> parenthesize $ toTree pat
--      H.PatternRecord (H.Pattern_Record ) ->
      H.PatternTuple pats -> htuple $ toTree <$> pats
--      H.PatternTyped (H.Pattern_Typed ) ->
      H.PatternWildcard -> cst "_"

instance ToTree H.Pattern_Application where
  toTree (H.Pattern_Application name pats) = spaceSep $ toTree name:(toTree <$> pats)

haskellSerdeDebug :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) CT.Expr)
haskellSerdeDebug context typ = do
  coder <- haskellCoder context typ
  return $ unidirectionalStep (fmap toTree . stepOut coder)

haskellSerdeStr :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) String)
haskellSerdeStr context typ = do
  coder <- haskellCoder context typ
  return $ unidirectionalStep (fmap (printExpr . parenthesize . toTree) . stepOut coder)

writeQName :: H.QualifiedName -> String
writeQName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ qualifiers ++ [unqual]
