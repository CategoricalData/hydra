{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Tabular where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


tabularModule :: Module
tabularModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A simple, untyped tabular data model, suitable for CSVs and TSVs")
  where
    ns = Namespace "hydra.ext.tabular"
    def = datatype ns
    tabular = typeref ns

    elements = [

      def "DataRow" $
        doc "A data row, containing optional-valued cells; one per column" $
        forAll "v" $ wrap $ list $ optional "v",

      def "HeaderRow" $
        doc "A header row, containing column names (but no types or data)" $
        wrap $ list string,

      def "Table" $
        doc "A simple table as in a CSV file, having an optional header row and any number of data rows" $
        forAll "v" $ record [
          "header">:
            doc "The optional header row of the table. If present, the header must have the same number of cells as each data row." $
            optional $ tabular "HeaderRow",
          "data">:
            doc "The data rows of the table. Each row must have the same number of cells." $
            list (tabular "DataRow" @@ "v")]]
