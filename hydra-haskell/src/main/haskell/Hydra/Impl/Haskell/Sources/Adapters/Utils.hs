module Hydra.Impl.Haskell.Sources.Adapters.Utils (adaptersUtilsGraph) where

import Hydra.Core
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.Standard


_hydra_adapters_utils :: Name
_hydra_adapters_utils = "hydra/adapters/utils"

adaptersUtilsGraph :: Graph Meta
adaptersUtilsGraph = standardGraph _hydra_adapters_utils [
  describePrecision]

describePrecision :: Element Meta
describePrecision = standardFunction _hydra_adapters_utils "describePrecision"
  "Display numeric precision as a string"
  (nominalType _Precision) stringType $
  standardMatch _Precision stringType [
    (_Precision_arbitrary, constFunction $ stringValue "arbitrary-precision"),
    (_Precision_bits,
      lambda "bits" $ apply (primitive _strings_cat)
        (list [
          apply (primitive _literals_showInt32) $ variable "bits",
          stringValue "-bit"]))]
