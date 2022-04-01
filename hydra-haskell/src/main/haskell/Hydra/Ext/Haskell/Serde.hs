module Hydra.Ext.Haskell.Serde (
  dataGraphToHaskellString,
) where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Ext.Haskell.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Codetree.Print
import Hydra.Util.Codetree.Script
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Haskell.Ast as H
import Hydra.Ext.Haskell.Syntax

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


class ToTree a where
  toTree :: a -> CT.Expr

instance ToTree H.Alternative where
  toTree (H.Alternative pat rhs _) = ifx caseOp (toTree pat) (toTree rhs)

instance ToTree H.Constructor where
  toTree cons = case cons of
    H.ConstructorOrdinary (H.Constructor_Ordinary name types) -> spaceSep [
      toTree name,
      spaceSep (toTree <$> types)]
    H.ConstructorRecord (H.Constructor_Record name fields) -> spaceSep [
      toTree name,
      curlyBracesList True (toTree <$> fields)]

instance ToTree H.DataDeclaration_Keyword where
  toTree kw = case kw of
    H.DataDeclaration_KeywordData -> cst "data"
    H.DataDeclaration_KeywordNewtype -> cst "newtype"

instance ToTree H.Declaration where
  toTree decl = case decl of
    H.DeclarationData (H.DataDeclaration kw _ hd cons deriv) -> indentBlock (spaceSep [toTree kw, toTree hd]) $
        consLines
        ++ if L.null derivCat then [] else [spaceSep [cst "deriving", parenList (toTree <$> derivCat)]]
      where
        derivCat = L.concat deriv
        consLines = L.zipWith consLine cons [0..]
        consLine c i = spaceSep [symb, toTree c]
          where
            symb = cst $ if i == 0 then "=" else "|"
    H.DeclarationType (H.TypeDeclaration hd typ) -> spaceSep [cst "type", toTree hd, cst "=", toTree typ]
    H.DeclarationValueBinding vb -> toTree vb
    H.DeclarationTypedBinding (H.TypedBinding (H.TypeSignature name htype) vb) -> newlineSep [ -- TODO: local bindings
        ifx typeOp (toTree name) (toTree htype),
        toTree vb]

instance ToTree H.ValueBinding where
  toTree vb = case vb of
    H.ValueBindingSimple (H.ValueBinding_Simple pat rhs _) -> ifx defineOp (toTree pat) (toTree rhs)

instance ToTree H.DeclarationHead where
  toTree hd = case hd of
    H.DeclarationHeadApplication (H.DeclarationHead_Application fun op) -> spaceSep [toTree fun, toTree op]
--    H.DeclarationHeadParens ... ->
    H.DeclarationHeadSimple name -> toTree name

instance ToTree H.DeclarationWithComments where
  toTree (H.DeclarationWithComments body mc) = case mc of
      Nothing -> toTree body
      Just c -> newlineSep [cst $ toHaskellComments c, toTree body]
    where
      toHaskellComments c = L.intercalate "\n" $ (\l -> "-- " ++ l) <$> L.lines c

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
      H.ExpressionList exprs -> bracketList True $ toTree <$> exprs
      H.ExpressionParens expr' -> parenthesize $ toTree expr'
    --  H.ExpressionPrefixApplication Expression_PrefixApplication
    --  H.ExpressionRightSection Expression_Section
      H.ExpressionTuple exprs -> parenList $ toTree <$> exprs
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
  toTree (H.Expression_Lambda bindings inner) = ifx lambdaOp (prefix "\\" head) body
    where
      head = spaceSep (toTree <$> bindings)
      body = toTree inner

instance ToTree H.Field where
  toTree (H.Field name typ) = spaceSep [toTree name, cst "::", toTree typ]

instance ToTree H.Import where
  toTree (H.Import qual name mod _) = spaceSep $ Y.catMaybes [
      Just $ cst "import",
      if qual then Just (cst "qualified") else Nothing,
      Just $ cst name,
      (\m -> cst $ "as " ++ m) <$> mod]

instance ToTree H.Literal where
  toTree lit = cst $ case lit of
    H.LiteralChar c -> show $ C.chr $ fromIntegral c
    H.LiteralDouble d -> show d
    H.LiteralFloat f -> show f
    H.LiteralInt i -> show i
    H.LiteralInteger i -> show i
    H.LiteralString s -> show s

instance ToTree H.Module where
  toTree (H.Module mh imports decls) = doubleNewlineSep $
      headerLine ++ importLines ++ declLines
    where
      headerLine = Y.maybe [] (\h -> [toTree h]) mh
      declLines = toTree <$> decls
      importLines = [newlineSep $ toTree <$> imports | not (L.null imports)]

instance ToTree H.Name where
  toTree name = cst $ case name of
    H.NameImplicit qn -> "?" ++ writeQName qn
    H.NameNormal qn -> writeQName qn
    H.NameParens qn -> "(" ++ writeQName qn ++ ")"

instance ToTree H.ModuleHead where
  toTree (H.ModuleHead mname _) = spaceSep [cst "module", cst mname, cst "where"]

instance ToTree H.Pattern where
  toTree pat = case pat of
      H.PatternApplication app -> toTree app
--      H.PatternAs (H.Pattern_As ) ->
      H.PatternList pats -> bracketList True $ toTree <$> pats
      H.PatternLiteral lit -> toTree lit
      H.PatternName name -> toTree name
      H.PatternParens pat -> parenthesize $ toTree pat
--      H.PatternRecord (H.Pattern_Record ) ->
      H.PatternTuple pats -> parenList $ toTree <$> pats
--      H.PatternTyped (H.Pattern_Typed ) ->
      H.PatternWildcard -> cst "_"

instance ToTree H.Pattern_Application where
  toTree (H.Pattern_Application name pats) = spaceSep $ toTree name:(toTree <$> pats)

instance ToTree H.Type where
  toTree htype = case htype of
    H.TypeApplication (H.Type_Application lhs rhs) -> ifx appOp (toTree lhs) (toTree rhs)
    H.TypeFunction (H.Type_Function dom cod) -> ifx arrowOp (toTree dom) (toTree cod)
--  H.TypeInfix Type_Infix
    H.TypeList htype -> bracketList False [toTree htype]
--  H.TypeParens Type
    H.TypeTuple types -> parenList $ toTree <$> types
    H.TypeVariable name -> toTree name

dataGraphToHaskellString :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified String
dataGraphToHaskellString cx g = do
  hsmod <- dataGraphToHaskellModule cx g
  return $ printExpr $ parenthesize $ toTree hsmod

writeQName :: H.QualifiedName -> String
writeQName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ qualifiers ++ [unqual]
