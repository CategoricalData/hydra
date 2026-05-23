-- Note: this is an automatically generated file. Do not edit.
-- | Registry of Hydra's built-in type classes.

module Hydra.Classes where
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The equality type class: instances support structural equality.
equality :: Typing.TypeClass
equality = Typing.TypeClass {
  Typing.typeClassDescription = "Equality: instances support structural equality."}
-- | The ordering type class: instances support total ordering (and equality).
ordering :: Typing.TypeClass
ordering =
    Typing.TypeClass {
      Typing.typeClassDescription = "Ordering: instances support total ordering (and equality)."}
