module Hydra.Common where

import Hydra.Core


isType :: Eq m => Type m -> Bool
isType typ = typeTerm typ == TypeTermNominal _Type
