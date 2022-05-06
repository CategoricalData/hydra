module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core
import Hydra.Impl.Haskell.Extras


data Case a = Case (Field Meta) deriving Show

data Element a = Element Name (Program a)

data Fld a = Fld (Field Meta) deriving Show

data Program a = Program (Term Meta) deriving Show

data Ref a = Ref Name

data Var a = Var String

doc :: String -> Program a -> Program a
doc str (Program (Term d m)) = Program $ Term d (m {metaDescription = Just str})

element :: Name -> String -> Program a -> Element a
element ns name prog = Element (ns ++ "." ++ name) prog

program :: Expression Meta -> Program a
program e = Program $ term e

strip :: Program a -> Term Meta
strip (Program term) = term

stripField :: Fld a -> Field Meta
stripField (Fld field) = field

term :: Expression Meta -> Term Meta
term e = Term e dflt

typed :: Type Meta -> Program a -> Program a
typed t (Program (Term d m)) = Program $ Term d (m {metaType = Just t})
