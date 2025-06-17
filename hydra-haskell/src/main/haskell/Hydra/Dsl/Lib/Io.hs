module Hydra.Dsl.Lib.Io where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


showFloat :: TTerm FloatValue -> TTerm String
showFloat = primitive1 _io_showFloat

showInteger :: TTerm IntegerValue -> TTerm String
showInteger = primitive1 _io_showInteger

showList :: TTerm (a -> String) -> TTerm [a] -> TTerm String
showList showElem xs = primitive2 _io_showList showElem xs

showLiteral :: TTerm Literal -> TTerm String
showLiteral = primitive1 _io_showLiteral

showTerm :: TTerm Term -> TTerm String
showTerm = primitive1 _io_showTerm

showType :: TTerm Type -> TTerm String
showType = primitive1 _io_showType
