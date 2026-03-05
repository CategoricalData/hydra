-- | A loose collection of tools for working with tabular data, including CSVs.

module Hydra.Ext.Tools.Tabular (
  referenceCoder,
  tabularAdapter,
) where

import Hydra.Kernel
import Hydra.Tabular
import qualified Hydra.Show.Core as ShowCore

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


-- | Consumes a map of field names to value coders, producing a record coder.
--   The record coder maps records with concrete values on the left, to records with references on the right, and vice versa.
--   Fields not designated in the map are unaffected; they are simply copied as-is in either direction.
--   For fields designated in the map, terms on the right *must* be variables.
referenceCoder :: (M.Map Name (Coder Term Name)) -> Coder Record Record
referenceCoder fieldCoders = Coder encode decode
  where
    encode cx (Record tname fields) = Record tname <$> CM.sequence (encodeField <$> fields)
      where
        encodeField field@(Field fname fterm) = case M.lookup fname fieldCoders of
          Just coder -> Field fname <$> (TermVariable <$> coderEncode coder cx fterm)
          Nothing -> return field
    decode cx (Record tname fields) = Record tname <$> CM.sequence (decodeField <$> fields)
      where
        decodeField field@(Field fname fterm) = case M.lookup fname fieldCoders of
          Just coder -> case fterm of
            TermVariable v -> Field fname <$> coderDecode coder cx v
            _ -> Left $ InContext (OtherError $ "expected variable, found: " ++ ShowCore.term fterm) cx
          Nothing -> return field

-- | Consumes a row type and a cell-level coder, producing a record coder.
--   The record coder maps data rows on the left to records on the right, and vice versa.
tabularAdapter :: RowType -> (Type -> Coder (Maybe v) Term) -> Coder (DataRow v) Record
tabularAdapter (RowType typeName fieldTypes) cellAdapter = Coder encode decode
  where
    cellCoders = cellAdapter <$> (fieldTypeType <$> fieldTypes)
    encode cx (DataRow cells) = do
      values <- CM.zipWithM (\coder cell -> coderEncode coder cx cell) cellCoders cells
      let fields = L.zipWith Field (fieldTypeName <$> fieldTypes) values
      return $ Record typeName fields
    decode cx (Record tname fields) = do
      if (tname /= typeName)
        then Left $ InContext (OtherError $ "expected record of type " ++ unName typeName ++ ", found record of type " ++ unName tname) cx
        else DataRow <$> CM.sequence (fieldDecoders cx <*> fields)
    fieldDecoders cx = L.zipWith (decodeField cx) fieldTypes cellCoders
      where
        decodeField cx fieldType coder field = if (fieldName field /= fieldTypeName fieldType)
          then Left $ InContext (OtherError $ "expected field " ++ unName (fieldTypeName fieldType) ++ ", found field " ++ unName (fieldName field)) cx
          else coderDecode coder cx (fieldTerm field)
