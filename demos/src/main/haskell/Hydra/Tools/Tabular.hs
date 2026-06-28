-- | A loose collection of tools for working with tabular data, including CSVs.

module Hydra.Tools.Tabular (
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
referenceCoder :: (M.Map Name (Coder Term Name Error)) -> Coder Record Record Error
referenceCoder fieldCoders = Coder encode decode
  where
    encode (Record tname fields) = Record tname <$> CM.sequence (encodeField <$> fields)
      where
        encodeField field@(Field fname fterm) = case M.lookup fname fieldCoders of
          Just coder -> Field fname <$> (TermVariable <$> coderEncode coder fterm)
          Nothing -> return field
    decode (Record tname fields) = Record tname <$> CM.sequence (decodeField <$> fields)
      where
        decodeField field@(Field fname fterm) = case M.lookup fname fieldCoders of
          Just coder -> case fterm of
            TermVariable v -> Field fname <$> coderDecode coder v
            _ -> Left $ ErrorOther (OtherError $ "expected variable, found: " ++ ShowCore.term fterm)
          Nothing -> return field

-- | Consumes a type name and field types plus a cell-level coder, producing a record coder.
--   The record coder maps data rows on the left to records on the right, and vice versa.
tabularAdapter :: Name -> [FieldType] -> (Type -> Coder (Maybe v) Term Error) -> Coder (DataRow v) Record Error
tabularAdapter typeName fieldTypes cellAdapter = Coder encode decode
  where
    cellCoders = cellAdapter <$> (fieldTypeType <$> fieldTypes)
    encode (DataRow cells) = do
      values <- CM.zipWithM (\coder cell -> coderEncode coder cell) cellCoders cells
      let fields = L.zipWith Field (fieldTypeName <$> fieldTypes) values
      return $ Record typeName fields
    decode (Record tname fields) = do
      if (tname /= typeName)
        then Left $ ErrorOther (OtherError $ "expected record of type " ++ unName typeName ++ ", found record of type " ++ unName tname)
        else DataRow <$> CM.sequence (fieldDecoders <*> fields)
    fieldDecoders = L.zipWith decodeField fieldTypes cellCoders
      where
        decodeField fieldType coder field = if (fieldName field /= fieldTypeName fieldType)
          then Left $ ErrorOther (OtherError $ "expected field " ++ unName (fieldTypeName fieldType) ++ ", found field " ++ unName (fieldName field))
          else coderDecode coder (fieldTerm field)
