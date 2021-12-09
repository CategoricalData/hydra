module Hydra.Impl.Haskell.Sources.Adapters.Utils where

import Hydra.Core
import Hydra.Lib.Literals
import Hydra.Lib.Strings
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Dsl.Elements


_hydra_adapters_utils :: Name
_hydra_adapters_utils = "hydra/adapters/utils"

adaptersUtilsGraph :: Graph Meta
adaptersUtilsGraph = standardGraph _hydra_adapters_utils [
  hsDescribePrecision]
  
hsDescribePrecision :: Context Meta -> Element Meta
hsDescribePrecision cx = standardFunction cx _hydra_adapters_utils "describePrecision"
  "Display numeric precision as a string"
  (nominalType _Precision) stringType $
  nominalMatch cx _Precision stringType [
    (_Precision_arbitrary, constFunction $ stringValue "arbitrary-precision"),
    (_Precision_bits,
      lambda "bits" $ apply
        (apply (primitive _strings_cat) (apply (primitive _literals_showInt32) $ variable "bits"))
        (stringValue "-bit"))]
