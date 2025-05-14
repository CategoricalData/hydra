module Hydra.Dsl.Lib.Io where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


showTerm :: TTerm Term -> TTerm String
showTerm = primitive1 _io_showTerm

showType :: TTerm Type -> TTerm String
showType = primitive1 _io_showType
