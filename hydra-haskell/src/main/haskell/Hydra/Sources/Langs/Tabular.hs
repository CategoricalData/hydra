{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tabular where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Types as Types


tabularModule :: Module Kv
tabularModule = Module ns elements [] $
    Just ("A simple, untyped tabular data model, suitable for CSVs and TSVs")
  where
    ns = Namespace "hydra/langs/tabular"
    def = datatype ns
    tabular = typeref ns

    elements = [
      def "DataRow" $
        doc "A data row, containing untyped cells; one per column" $
        list string,

      def "HeaderRow" $
        doc "A header row, containing column names (but no types or data)" $
        list string,

      def "Table" $
        record [
          "header">: optional $ tabular "HeaderRow",
          "data">: list $ tabular "DataRow"]]
