module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Meta


data Case a = Case (Field Meta) deriving Show

data Element a = Element Name (Program a)

data Fld a = Fld (Field Meta) deriving Show

data Program a = Program (Data Meta) deriving Show

data Ref a = Ref Name

data Var a = Var String

doc :: String -> Program a -> Program a
doc str (Program (Data d m)) = Program $ Data d (setDescription (Just str) m)

element :: Name -> String -> Program a -> Element a
element ns name prog = Element (ns ++ "." ++ name) prog

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
