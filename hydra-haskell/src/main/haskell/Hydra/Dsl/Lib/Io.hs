module Hydra.Dsl.Lib.Io where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


showTerm :: TTerm (Term -> String)
showTerm = TTerm $ Terms.primitive _io_showTerm

showType :: TTerm (Type -> String)
showType = TTerm $ Terms.primitive _io_showType
