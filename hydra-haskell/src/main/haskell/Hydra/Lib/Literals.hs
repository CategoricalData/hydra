module Hydra.Lib.Literals where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims
import Hydra.Impl.Haskell.Dsl.Terms


_hydra_lib_literals :: Name
_hydra_lib_literals = "hydra/lib/literals"

_literals_showInt32 :: Name
_literals_showInt32 = qname _hydra_lib_literals "showInt32"

_literals_showString :: Name
_literals_showString = qname _hydra_lib_literals "showString"

hsShowInt32 :: Int -> String
hsShowInt32 = show

hsShowString :: String -> String
hsShowString = show

hydraLibLiteralsPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibLiteralsPrimitives = [
    prim1 _literals_showInt32 int32Input stringOutput hsShowInt32,
    prim1 _literals_showString stringInput stringOutput hsShowString]
