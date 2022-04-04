module Hydra.Impl.Haskell.Sources.Errors where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard

-- Note: here, the element namespace doubles as a graph name
hydraErrorsName = "hydra/errors"

hydraErrors :: Graph Meta
hydraErrors = Graph hydraErrorsName elements (const True) hydraCoreName
  where
    errorsDef = datatype hydraErrorsName
    elements = [

      errorsDef "Qualified"
        "An optional value qualified with a (possibly empty) list of warnings" $
        universal "m" $ record [
          field "value" (optional $ variable "m"),
          field "warnings" (list string)]]
