module Hydra.Impl.Haskell.Dsl.Elements where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms


typeElement :: Context Meta -> Name -> Type -> Element Meta
typeElement cx name typ = Element {
  elementName = name,
  elementSchema = defaultTerm $ ExpressionElement _Type,
  elementData = encodeType cx typ}
