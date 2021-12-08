module Hydra.Lib.Literals where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims
import Hydra.Impl.Haskell.Dsl.Terms


_hydra_lib_literals :: Name
_hydra_lib_literals = "hydra/lib/literals"

_showInt32 :: Name
_showInt32 = qname _hydra_lib_literals "showInt32"

_showString :: Name
_showString = qname _hydra_lib_literals "showString"

hsShowInt32 :: Int -> String
hsShowInt32 = show

hsShowString :: String -> String
hsShowString = show

hydraLibLiteralsPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibLiteralsPrimitives = [
    prim1 _showInt32 int32Input stringOutput hsShowInt32,
    prim1 _showString stringInput stringOutput hsShowString]
