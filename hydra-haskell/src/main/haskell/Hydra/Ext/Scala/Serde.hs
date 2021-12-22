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

writeImportExportStat :: Scala.ImportExportStat -> CT.Expr
writeImportExportStat ie = case ie of
  Scala.ImportExportStatImport (Scala.Import importers) -> newlineSep (writeImporter <$> importers)
--  Scala.ImportExportStatExport exp ->

writeImporter :: Scala.Importer -> CT.Expr
writeImporter (Scala.Importer (Scala.Term_RefName (Scala.Term_Name ref)) importees) = newlineSep (write <$> importees)
  where
    write it = spaceSep [cst "import", cst (ref ++ forImportee it)]
    forImportee it = case it of
      Scala.ImporteeWildcard -> ".*"
      Scala.ImporteeName (Scala.Importee_Name (Scala.NameValue name)) -> "." ++ name

writePkg :: Scala.Pkg -> CT.Expr
writePkg (Scala.Pkg (Scala.Term_Name name) _ stats) = doubleNewlineSep $ package:(writeStat <$> stats)
  where
    package = spaceSep [cst "package", cst name]

writeStat :: Scala.Stat -> CT.Expr
writeStat stat = case stat of
--  Scala.StatTerm Term ->
--  Scala.StatDecl Decl ->
--  Scala.StatDefn Defn ->
  Scala.StatImportExport ie -> writeImportExportStat ie
