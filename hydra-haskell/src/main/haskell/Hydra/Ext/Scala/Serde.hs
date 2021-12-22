module Hydra.Ext.Scala.Serde (
  dataGraphToScalaString,
) where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Ext.Scala.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Codetree.Print
import Hydra.Util.Codetree.Script
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Scala.Meta as Scala

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


dataGraphToScalaString :: (Default a, Ord a, Read a, Show a) => Context a -> Graph a -> Qualified String
dataGraphToScalaString cx g = do
  pkg <- dataGraphToScalaPackage cx g
  return $ printExpr $ parenthesize $ writePkg pkg

writePkg :: Scala.Pkg -> CT.Expr
writePkg (Scala.Pkg name _ stats) = cst "TODO"
