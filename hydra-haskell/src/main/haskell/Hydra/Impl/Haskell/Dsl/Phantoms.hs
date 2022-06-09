module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core
import Hydra.Evaluation
import qualified Hydra.Graph as Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Meta


newtype Case a = Case (Field Meta) deriving Show

data Element a = Element Name (Program a)

newtype Fld a = Fld (Field Meta) deriving Show

newtype Program a = Program (Data Meta) deriving Show

newtype Ref a = Ref Name

newtype Var a = Var String

doc :: String -> Program a -> Program a
doc str (Program (Data d m)) = Program $ Data d (setDescription (Just str) m)

element :: Graph.GraphName -> Name -> Program a -> Element a
element (Graph.GraphName ns) (Name name) prog = Element (Name $ ns ++ "." ++ name) prog

program :: DataTerm Meta -> Program a
program e = Program $ term e

strip :: Program a -> Data Meta
strip (Program term) = term

stripField :: Fld a -> Field Meta
stripField (Fld field) = field

term :: DataTerm Meta -> Data Meta
term e = Data e dflt

typed :: Context Meta -> Type Meta -> Program a -> Program a
typed cx t (Program (Data d m)) = Program $ Data d (setType cx (Just t) m)
