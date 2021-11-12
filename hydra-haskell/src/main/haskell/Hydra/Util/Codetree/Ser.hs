module Hydra.Util.Codetree.Ser (
  codeTreeToString,
) where

import Hydra.Util.Codetree.Ast

import qualified Data.List as L
import qualified Data.Maybe as Y


class Ser a where
  write :: a -> String

instance Ser Expression where
  write expr = case expr of
      ExpressionAtomic s -> s
      ExpressionBlock (Expression_Block (Delimiters open close) head subs) -> unlines $
        [unwords $ Y.catMaybes [write <$> head, open]]
        ++ (indent . write <$> subs)
        ++ Y.maybeToList close
      ExpressionInfix (Expression_Operator (Operator symbol prec assoc) ops) -> unwords $
        L.intersperse symbol $ parens . write <$> ops
      ExpressionList (Expression_List sep exprs) -> if hasNewlines
          then L.intercalate (sep ++ "\n") $ indent <$> strings
          else L.intercalate sep strings
        where
          hasNewlines = any (L.elem '\n') strings
          strings = write <$> exprs
      ExpressionPrefix (Expression_Operator (Operator symbol prec assoc) ops) -> unwords $ symbol:(write <$> ops)

codeTreeToString :: Expression -> String
codeTreeToString = write

indent :: String -> String
indent s = L.intercalate "\n" $ ("  " ++) <$> lines s

parens :: String -> String
parens s = "(" ++ s ++ ")"
