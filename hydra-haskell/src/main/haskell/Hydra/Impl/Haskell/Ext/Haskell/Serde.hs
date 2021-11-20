module Hydra.Impl.Haskell.Ext.Haskell.Serde (
  haskellSerdeDebug,
  haskellSerdeStr,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Ext.Haskell.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Codetree.Ser
import Hydra.Impl.Haskell.Dsl
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Haskell.Ast as H

import qualified Data.Char as C
import qualified Data.List as L


class ToTree a where
  toTree :: a -> CT.Expression

instance ToTree H.Alternative where
  toTree (H.Alternative pat rhs _) = listSpace [toTree pat, atom "->", toTree rhs]

instance ToTree H.Expression where
  toTree expr = case expr of
      H.ExpressionApplication app -> toTree app
      H.ExpressionCase cases -> toTree cases
      H.ExpressionConstructRecord r -> toTree r
      H.ExpressionDo statements -> indentBlock (Just $ atom "do") $ toTree <$> statements
      H.ExpressionIf ifte -> toTree ifte
    --  H.ExpressionInfixApplication Expression_InfixApplication
      H.ExpressionLiteral lit -> toTree lit
      H.ExpressionLambda lam -> toTree lam
    --  H.ExpressionLeftSection Expression_Section
    --  H.ExpressionLet Expression_Let
      H.ExpressionList exprs -> hlist $ toTree <$> exprs
      H.ExpressionParens expr' -> hparens $ toTree expr'
    --  H.ExpressionPrefixApplication Expression_PrefixApplication
    --  H.ExpressionRightSection Expression_Section
      H.ExpressionTuple exprs -> htuple $ toTree <$> exprs
    --  H.ExpressionTypeSignature Expression_TypeSignature
    --  H.ExpressionUpdateRecord Expression_UpdateRecord
      H.ExpressionVariable name -> toTree name

instance ToTree H.Expression_Application where
  toTree (H.Expression_Application fun arg) = listSpace [toTree fun, toTree arg]

instance ToTree H.Expression_Case where
  toTree (H.Expression_Case cs alts) = indentBlock (Just $ listSpace [atom "case", toTree cs, atom "of"]) $ toTree <$> alts

instance ToTree H.Expression_ConstructRecord where
  toTree (H.Expression_ConstructRecord name updates) = indentCurly (Just $ toTree name) $ fromUpdate <$> updates
    where
      fromUpdate (H.FieldUpdate fn val) = listSpace [toTree fn, atom "=", toTree val]

instance ToTree H.Expression_If where
  toTree (H.Expression_If eif ethen eelse) = CT.ExpressionBlock $ CT.Expression_Block noDelimiters
    (Just $ listSpace [atom "if", toTree eif])
    [listSpace [atom "then", toTree ethen],
     listSpace [atom "else", toTree eelse]]

instance ToTree H.Expression_Lambda where
  toTree (H.Expression_Lambda bindings inner) = listSpace [head, atom "->", toTree inner]
    where
      head = listCat [atom "\\", listSpace (toTree <$> bindings)]

instance ToTree H.Literal where
  toTree lit = CT.ExpressionAtomic $ case lit of
    H.LiteralChar c -> show $ C.chr $ fromIntegral c
    H.LiteralDouble d -> show d
    H.LiteralFloat f -> show f
    H.LiteralInt i -> show i
    H.LiteralInteger i -> show i
    H.LiteralString s -> show s

instance ToTree H.Name where
  toTree name = atom $ case name of
    H.NameImplicit qn -> "?" ++ writeQName qn
    H.NameNormal qn -> writeQName qn
    H.NameParens qn -> "(" ++ writeQName qn ++ ")"

instance ToTree H.Pattern where
  toTree pat = case pat of
      H.PatternApplication app -> toTree app
--      H.PatternAs (H.Pattern_As ) ->
      H.PatternList pats -> hlist $ toTree <$> pats
      H.PatternLiteral lit -> toTree lit
      H.PatternName name -> toTree name
      H.PatternParens pat -> hparens $ toTree pat
--      H.PatternRecord (H.Pattern_Record ) ->
      H.PatternTuple pats -> htuple $ toTree <$> pats
--      H.PatternTyped (H.Pattern_Typed ) ->
      H.PatternWildcard -> atom "_"

instance ToTree H.Pattern_Application where
  toTree (H.Pattern_Application name pats) = listSpace $ toTree name:(toTree <$> pats)

atom :: String -> CT.Expression
atom = CT.ExpressionAtomic

haskellSerdeDebug :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) CT.Expression)
haskellSerdeDebug context typ = do
  coder <- haskellCoder context typ
  return $ unidirectionalStep (fmap toTree . stepOut coder)

haskellSerdeStr :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Type -> Qualified (Step (Term a) String)
haskellSerdeStr context typ = do
  coder <- haskellCoder context typ
  return $ unidirectionalStep (fmap (codeTreeToString . toTree) . stepOut coder)

hlist :: [CT.Expression] -> CT.Expression
hlist exprs = listCat [atom "[", listCommas exprs, atom "]"]

hparens :: CT.Expression -> CT.Expression
hparens expr = htuple [expr]

htuple :: [CT.Expression] -> CT.Expression
htuple exprs = listCat [atom "(", listCommas exprs, atom ")"]

indentBlock :: Maybe CT.Expression -> [CT.Expression] -> CT.Expression
indentBlock head exprs = CT.ExpressionBlock $ CT.Expression_Block noDelimiters head exprs

indentCurly :: Maybe CT.Expression -> [CT.Expression] -> CT.Expression
indentCurly head exprs = CT.ExpressionBlock $ CT.Expression_Block (CT.Delimiters (Just "{") (Just "}")) head exprs

listCat :: [CT.Expression] -> CT.Expression
listCat l = CT.ExpressionList $ CT.Expression_List "" l

listCommas :: [CT.Expression] -> CT.Expression
listCommas l = CT.ExpressionList $ CT.Expression_List ", " l

listSpace :: [CT.Expression] -> CT.Expression
listSpace l = CT.ExpressionList $ CT.Expression_List " " l

noDelimiters :: CT.Delimiters
noDelimiters = CT.Delimiters Nothing Nothing

writeQName :: H.QualifiedName -> String
writeQName (H.QualifiedName qualifiers unqual) = L.intercalate "." $ qualifiers ++ [unqual]
