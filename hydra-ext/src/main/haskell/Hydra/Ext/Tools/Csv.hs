-- | A collection of tools for working with comma-separated values (CSVs).

module Hydra.Ext.Tools.Csv where

import Hydra.Kernel
import Hydra.Tabular
import qualified Hydra.Relational as RM
import Hydra.Ext.Tools.Tabular

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


-- Untyped tables --------------------------------------------------------------

-- | Encodes a single DataRow as a line of a CSV, and vice versa.
--   No support for optional cells; only empty strings. No attempt is made to check that rows are of a consistent width.
dataRowCsvCoder :: Coder (DataRow String) String
dataRowCsvCoder = Coder encode decode
  where
    encode _cx (DataRow cells) = Right $ encodeCsvLine $ Y.catMaybes cells
    decode _cx line = Right $ DataRow $ fmap Just $ decodeCsvLine line

tableCsvCoder :: Bool -> Coder (Table String) [String]
tableCsvCoder hasHeader = Coder encode decode
  where
    encode cx (Table mheader rows) = do
        hrows <- headerRows
        drows <- CM.mapM (coderEncode dataRowCsvCoder cx) rows
        return $ hrows ++ drows
      where
        headerRows = if hasHeader
          then case mheader of
            Just (HeaderRow names) -> Right [encodeCsvLine names]
            Nothing -> Left $ InContext (OtherError "missing header") cx
          else Right []
    decode cx rows = do
        (mheader, rest) <- if hasHeader
          then if L.null rows
            then Left $ InContext (OtherError "missing header") cx
            else Right (Just $ HeaderRow $ decodeCsvLine $ head rows, tail rows)
          else Right (Nothing, rows)
        drows <- CM.mapM (coderDecode dataRowCsvCoder cx) rest
        return $ Table mheader drows


-- Relational tables -----------------------------------------------------------

type DomainCoders t v = M.Map t (Coder v String)

relationCsvCoder :: (Ord t, Show t) => DomainCoders t v -> RM.RelationSchema t -> Bool -> Context -> Either (InContext OtherError) (Coder (RM.Relation v) [String])
relationCsvCoder coderMap schema hasHeader cx = do
    coder <- rowCsvCoder coderMap schema cx
    return $ Coder (encode coder) (decode coder)
  where
    encode coder cx (RM.Relation rows) = do
        dataRows <- CM.mapM (coderEncode coder cx) rows
        return $ headerRows ++ dataRows
      where
        headerRows = if hasHeader
          then [encodeCsvLine (RM.unColumnName . RM.columnSchemaName <$> (RM.relationSchemaColumns schema))]
          else []
    decode coder cx inRows = do
        outRows <- CM.mapM (coderDecode coder cx) dataRows
        return $ RM.Relation outRows
      where
        dataRows = if hasHeader then L.tail inRows else inRows

-- | Encodes a single Row as a line of a CSV, and vice versa.
rowCsvCoder :: (Ord t, Show t) => DomainCoders t v -> RM.RelationSchema t -> Context -> Either (InContext OtherError) (Coder (RM.Row v) String)
rowCsvCoder coderMap schema cx = do
    coders <- CM.mapM findCoder (RM.columnSchemaDomain <$> RM.relationSchemaColumns schema)
    return $ Coder (encode coders) (decode coders)
  where
    encode coders cx' (RM.Row cells) = encodeCsvLine <$> (encodeCells cx' coders cells)
    decode coders cx' line = RM.Row <$> (decodeCells cx' coders $ decodeCsvLine line)
    findCoder typ = case M.lookup typ coderMap of
      Nothing -> Left $ InContext (OtherError $ "no coder for type: " ++ show typ) cx
      Just c -> pure c
    encodeCells cx' coders = CM.zipWithM (\c v -> coderEncode c cx' v) coders
    decodeCells cx' coders = CM.zipWithM (\c v -> coderDecode c cx' v) coders

stringCoders :: DomainCoders () String
stringCoders = M.singleton () $ Coder encode decode
  where
    encode _cx s = Right s
    decode _cx s = Right s

stringSchema :: Int -> RM.RelationSchema ()
stringSchema len = RM.RelationSchema (RM.RelationName $ "StringTable" ++ show len) (colSchema <$> [1..len]) [] []
  where
    colSchema i = RM.ColumnSchema (RM.ColumnName $ "Column" ++ show i) ()

-- Common functions ------------------------------------------------------------

decodeCsvCell :: String -> String
decodeCsvCell ('"':xs) = decodeCsvQuotes (init xs)  -- Remove surrounding quotes
decodeCsvCell cell = cell  -- If not quoted, return as is

decodeCsvLine :: String -> [String]
decodeCsvLine xs = fmap decodeCsvCell (splitCsvLine xs)

decodeCsvQuotes :: String -> String
decodeCsvQuotes [] = []
decodeCsvQuotes ('"':'"':ys) = '"' : decodeCsvQuotes ys  -- Handle escaped quotes
decodeCsvQuotes (y:ys) = y : decodeCsvQuotes ys

encodeCsvCell :: String -> String
encodeCsvCell cell
  | any (`elem` cell) [',', '\n', '"'] = '"' : escapeCsvQuotes cell ++ "\""
  | otherwise = cell

encodeCsvLine :: [String] -> String
encodeCsvLine = L.intercalate "," . fmap encodeCsvCell

escapeCsvQuotes :: String -> String
escapeCsvQuotes [] = []
escapeCsvQuotes (x:xs)
  | x == '"'  = '"' : '"' : escapeCsvQuotes xs
  | otherwise = x : escapeCsvQuotes xs

splitCsvLine :: String -> [String]
splitCsvLine [] = []
splitCsvLine ('"':xs) = ('"':quoted ++ "\"") : splitCsvLine next
  where
    (quoted, rest) = span (/= '"') xs
    afterQuote = drop 1 rest  -- Skip closing quote
    next = dropWhile (== ',') afterQuote  -- Skip comma
splitCsvLine xs = cell : splitCsvLine (drop 1 rest)  -- Skip comma
  where
    (cell, rest) = break (== ',') xs
